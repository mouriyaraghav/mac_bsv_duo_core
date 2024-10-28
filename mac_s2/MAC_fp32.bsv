package MAC_fp32;
// importing packages
import DReg::*;

// Result structure for Ripple Carry Adder
typedef struct {
    Bit#(1) overflow;
    Bit#(7) sum;
} RCA7_result deriving(Bits, Eq);

typedef struct {
    Bit#(1) overflow;
    Bit#(8) sum;
} RCA8_result deriving(Bits, Eq);

typedef struct {
    Bit#(1) overflow;
    Bit#(9) sum;
} RCA9_result deriving(Bits, Eq);

typedef struct {
    Bit#(1) overflow;
    Bit#(24) sum;
} RCA24_result deriving(Bits, Eq);

typedef struct {
    Bit#(1) overflow;
    Bit#(25) sum;
} RCA25_result deriving(Bits, Eq);

typedef struct {
    Bit#(1) overflow;
    Bit#(26) sum;
} RCA26_result deriving(Bits, Eq);

typedef struct {
    Bit#(1) overflow;
    Bit#(48) sum;
} RCA48_result deriving(Bits, Eq);



// Result structure for Full Adder
typedef struct {
    Bit#(1) carryOut;
    Bit#(1) sum;
} FA_result deriving(Bits, Eq);

typedef struct {
    Bit#(7) carryOut;
    Bit#(7) sum;
} AA7_result deriving(Bits, Eq);

typedef struct {
    Bit#(32) val;
    Bool     overflow;
} Multf_result deriving(Bits, Eq);


typedef struct {
    Bit#(32) val;
    Bit#(16) product;
    Bool     add_overflow;
    Bool     mult_overflow;
} MACf32_result deriving(Bits, Eq);

typedef struct {
    Bit#(1)  sign_max;
    Bool     opp_sign;
    Bool    finish;
    RCA48_result rca48ans;
    Bit#(48) manf_extended;
    Bit#(48) manD;
    Bit#(48) manE;
    Bit#(24) final_mant;
    Bit#(8)  exp_final;
    Bit#(8)  final_exp;
} Addf_par_data deriving(Bits, Eq);


typedef struct {
    Bit#(32)          val;
    Bit#(48)          manE;
    Bit#(48)          manD;
    Bit#(48)        add_inter;
    Bit#(2)         indicator;
    Bool     exp_overflow;
    Bit#(8)     shift_num;
    Int#(8)     sub_count;
} Adderfp32_result deriving(Bits, Eq);


// Full Adder Interface
interface FA_ifc;
    method FA_result result(Bit#(1) a, Bit#(1) b, Bit#(1) cin);
endinterface : FA_ifc

// Ripple Carry Adder Interface
interface RCA8_ifc;
    method RCA8_result result(Bit#(8) a, Bit#(8) b, Bit#(1) cin);
endinterface : RCA8_ifc

interface RCA9_ifc;
    method RCA9_result result(Bit#(9) a, Bit#(9) b, Bit#(1) cin);
endinterface : RCA9_ifc

interface RCA24_ifc;
    method RCA24_result result(Bit#(24) a, Bit#(24) b, Bit#(1) cin);
endinterface : RCA24_ifc

interface RCA25_ifc;
    method RCA25_result result(Bit#(25) a, Bit#(25) b, Bit#(1) cin);
endinterface : RCA25_ifc

interface RCA26_ifc;
    method RCA26_result result(Bit#(26) a, Bit#(26) b, Bit#(1) cin);
endinterface : RCA26_ifc

interface RCA48_ifc;
    method RCA48_result result(Bit#(48) a, Bit#(48) b, Bit#(1) cin);
endinterface : RCA48_ifc


interface RCA7_ifc;
    method RCA7_result result(Bit#(7) a, Bit#(7) b, Bit#(1) cin);
endinterface : RCA7_ifc

interface DF1_ifc;
    method Action start(Bit#(48) a);
    method Maybe#(Int#(8)) result();
endinterface : DF1_ifc

// Array Adders interfaces
interface AA7_ifc;
    method AA7_result result(Bit#(7) a, Bit#(7) b, Bit#(7) cin);
endinterface : AA7_ifc
// Int8 Multiplier Interface
// interface Multf_ifc;
//     method Action start(Bit#(16) a, Bit#(16) b);
//     method Multf_result result();
// endinterface : Multf_ifc

interface MACf32_ifc;
    method Action start(Bit#(16) a, Bit#(16) b, Bit#(32) c);
    method Bit#(32) result();
endinterface : MACf32_ifc


interface Adderfp32_ifc;
    method Action start(Bit#(32) a, Bit#(32) b);
    method Maybe#(Adderfp32_result) result();
endinterface : Adderfp32_ifc



module mkRippleCarryAdder7(RCA7_ifc);
    function FA_result fulladdition(Bit#(1) a, Bit#(1) b, Bit#(1) cin);
        FA_result res = FA_result{
            carryOut : (a & b) | (b & cin) | (cin & a),
            sum      : a ^ b ^ cin
        };
        return res;
    endfunction : fulladdition

    function RCA7_result rippleCarryAddition7(Bit#(7) a, Bit#(7) b, Bit#(1) cin);
        Bit#(7) sum;
        Bit#(8) carry = 'b0;

        carry[0] = cin;
        for(Integer i = 0;i<7;i=i+1) begin
            FA_result r = fulladdition(a[i],b[i],carry[i]);
            sum[i]     = r.sum;
            carry[i+1] = r.carryOut;
        end
        RCA7_result rca7_res = RCA7_result{
            overflow : carry[7],
            sum      : sum
        };
        // rca9_res.sum = sum;
        // rca9_res.overflow = carry[9];
        return rca7_res;
    endfunction : rippleCarryAddition7

    method RCA7_result result(Bit#(7) a, Bit#(7) b, Bit#(1) cin);
        return rippleCarryAddition7(a,b,cin);
    endmethod : result
endmodule : mkRippleCarryAdder7

module mkRippleCarryAdder8(RCA8_ifc);
    function RCA8_result rippleCarryAddition8 (Bit#(8) a,Bit#(8) b,Bit#(1) cin);
        Bit#(8) sum;
        Bit#(9) carry='b0;
        carry[0] = cin;

        for(Integer i = 0;i<8;i=i+1) begin
            sum[i]     = (a[i]^b[i]^carry[i]);
            carry[i+1] = (a[i]&b[i])|(b[i]&carry[i])|(carry[i]&a[i]);
        end
        RCA8_result rca8_res;
        rca8_res.sum = sum;
        rca8_res.overflow = carry[8];
        return rca8_res;
    endfunction : rippleCarryAddition8
    method RCA8_result result(Bit#(8) a, Bit#(8) b, Bit#(1) cin);
        return rippleCarryAddition8(a,b,cin);
    endmethod : result
endmodule : mkRippleCarryAdder8


module mkRippleCarryAdder9(RCA9_ifc);
    function RCA9_result rippleCarryAddition9 (Bit#(9) a,Bit#(9) b,Bit#(1) cin);
        Bit#(9) sum;
        Bit#(10) carry='b0;
        carry[0] = cin;

        for(Integer i = 0;i<9;i=i+1) begin
            sum[i]     = (a[i]^b[i]^carry[i]);
            carry[i+1] = (a[i]&b[i])|(b[i]&carry[i])|(carry[i]&a[i]);
        end
        RCA9_result rca9_res;
        rca9_res.sum = sum;
        rca9_res.overflow = carry[9];
        return rca9_res;
    endfunction : rippleCarryAddition9
    method RCA9_result result(Bit#(9) a, Bit#(9) b, Bit#(1) cin);
        return rippleCarryAddition9(a,b,cin);
    endmethod : result
endmodule : mkRippleCarryAdder9



module mkRippleCarryAdder24(RCA24_ifc);
    function RCA24_result rippleCarryAddition24 (Bit#(24) a,Bit#(24) b,Bit#(1) cin);
        Bit#(24) sum;
        Bit#(25) carry='b0;
        carry[0] = cin;

        for(Integer i = 0;i<24;i=i+1) begin
            sum[i]     = (a[i]^b[i]^carry[i]);
            carry[i+1] = (a[i]&b[i])|(b[i]&carry[i])|(carry[i]&a[i]);
        end
        RCA24_result rca24_res;
        rca24_res.sum = sum;
        rca24_res.overflow = carry[24];
        return rca24_res;
    endfunction : rippleCarryAddition24
    method RCA24_result result(Bit#(24) a, Bit#(24) b, Bit#(1) cin);
        return rippleCarryAddition24(a,b,cin);
    endmethod : result
endmodule : mkRippleCarryAdder24


module mkRippleCarryAdder25(RCA25_ifc);
    function RCA25_result rippleCarryAddition25 (Bit#(25) a,Bit#(25) b,Bit#(1) cin);
        Bit#(25) sum;
        Bit#(26) carry='b0;
        carry[0] = cin;

        for(Integer i = 0;i<25;i=i+1) begin
            sum[i]     = (a[i]^b[i]^carry[i]);
            carry[i+1] = (a[i]&b[i])|(b[i]&carry[i])|(carry[i]&a[i]);
        end
        RCA25_result rca25_res;
        rca25_res.sum = sum;
        rca25_res.overflow = carry[25];
        return rca25_res;
    endfunction : rippleCarryAddition25
    method RCA25_result result(Bit#(25) a, Bit#(25) b, Bit#(1) cin);
        return rippleCarryAddition25(a,b,cin);
    endmethod : result
endmodule : mkRippleCarryAdder25


module mkRippleCarryAdder26(RCA26_ifc);
    function RCA26_result rippleCarryAddition26 (Bit#(26) a,Bit#(26) b,Bit#(1) cin);
        Bit#(26) sum;
        Bit#(27) carry='b0;
        carry[0] = cin;

        for(Integer i = 0;i<26;i=i+1) begin
            sum[i]     = (a[i]^b[i]^carry[i]);
            carry[i+1] = (a[i]&b[i])|(b[i]&carry[i])|(carry[i]&a[i]);
        end
        RCA26_result rca26_res;
        rca26_res.sum = sum;
        rca26_res.overflow = carry[26];
        return rca26_res;
    endfunction : rippleCarryAddition26
    method RCA26_result result(Bit#(26) a, Bit#(26) b, Bit#(1) cin);
        return rippleCarryAddition26(a,b,cin);
    endmethod : result
endmodule : mkRippleCarryAdder26

module mkRippleCarryAdder48(RCA48_ifc);
    function RCA48_result rippleCarryAddition48 (Bit#(48) a,Bit#(48) b,Bit#(1) cin);
        Bit#(48) sum;
        Bit#(49) carry='b0;
        carry[0] = cin;

        for(Integer i = 0;i<48;i=i+1) begin
            sum[i]     = (a[i]^b[i]^carry[i]);
            carry[i+1] = (a[i]&b[i])|(b[i]&carry[i])|(carry[i]&a[i]);
        end
        RCA48_result rca48_res;
        rca48_res.sum = sum;
        rca48_res.overflow = carry[48];
        return rca48_res;
    endfunction : rippleCarryAddition48
    method RCA48_result result(Bit#(48) a, Bit#(48) b, Bit#(1) cin);
        return rippleCarryAddition48(a,b,cin);
    endmethod : result
endmodule : mkRippleCarryAdder48


module mkArrayAdder7(AA7_ifc);
    function FA_result fulladdition(Bit#(1) a, Bit#(1) b, Bit#(1) cin);
        FA_result res = FA_result{
            carryOut : (a & b) | (b & cin) | (cin & a),
            sum      : a ^ b ^ cin
        };
        return res;
    endfunction : fulladdition
    function AA7_result arrayAddition7(Bit#(7) a, Bit#(7) b, Bit#(7) cin);
        Bit#(7) sum;
        Bit#(7) carry;
        for(Integer i = 0;i<7;i=i+1) begin
            FA_result r = fulladdition(a[i],b[i],cin[i]);
            sum[i]   = r.sum;
            carry[i] = r.carryOut;
        end
        AA7_result res1 = AA7_result{
            carryOut : carry,
            sum      : sum
        };
        return res1;
    endfunction : arrayAddition7
    method AA7_result result(Bit#(7) a, Bit#(7) b, Bit#(7) cin);
        return arrayAddition7( a, b, cin);
    endmethod : result
endmodule : mkArrayAdder7



module mkDistOfOne(DF1_ifc);
    Reg#(Bit#(48)) rg_inpA  <- mkReg(0);
    Reg#(Bool) rg_inp_valid <- mkReg(False);

    Reg#(Int#(8)) rg_dist_out <- mkReg(unpack(0));
    Reg#(Bool)    rg_out_valid <- mkDReg(False);

    rule dist_rl(rg_inp_valid);
        if(rg_inpA[47]==1'b1)
        begin
            rg_out_valid <= True;
            rg_inp_valid <= False;
        end
        else
        begin
            rg_dist_out <= rg_dist_out + 1;
            rg_inpA <= rg_inpA <<1;
        end
    endrule : dist_rl

    method Action start(Bit#(48) a);
        rg_inpA      <= a;
        rg_inp_valid <= True;
    endmethod : start
    method Maybe#(Int#(8)) result();
        Maybe#(Int#(8)) x = tagged Invalid;
        if(rg_out_valid) begin
            x = tagged Valid rg_dist_out;
        end
        return x;
    endmethod : result
endmodule : mkDistOfOne


module mkAdder_fp32(Adderfp32_ifc);
    // Registers used
    Reg#(Bit#(32)) rg_inpA      <- mkReg(0);
    Reg#(Bit#(32)) rg_inpB      <- mkReg(0);

    Reg#(Bool)     rg_inp_valid  <- mkReg(False);
    Reg#(Bool)     rg_add1_valid <- mkReg(False);
    Reg#(Bool)     rg_add2_valid <- mkReg(False);
    
    Reg#(Bit#(32)) rg_add_out          <- mkReg(0);
    Reg#(Bit#(48)) rg_manD        <- mkReg(0);
    Reg#(Bit#(48)) rg_manE         <- mkReg(0);
    Reg#(Bit#(2))  rg_indicator   <- mkReg(0);
    Reg#(Bit#(48)) rg_add_out_over <- mkReg(0);
    Reg#(Bit#(8))  rg_shift_num        <- mkReg(0);
    Reg#(Int#(8))  rg_sub_count       <- mkReg(unpack(0));
    Reg#(Bool)     rg_add_out_valid    <- mkDReg(False);
    Reg#(Bool)     rg_add_out_overflow <- mkDReg(False);

    Reg#(RCA48_result) rg_rca48_out  <- mkReg(unpack(0));
    // Reg#(Bool)         rg_opp_sign   <- mkReg(False);
    Reg#(Addf_par_data) rg_addf_par <- mkReg(unpack(0));

    // Module instantiation
    // AA7_ifc aa7[7];
    // for(Integer i = 0;i < 7;i = i+1) begin
    //     aa7[i] <- mkArrayAdder7();
    // end

    // RCA7_ifc  rca7   <- mkRippleCarryAdder7();
    RCA8_ifc  rca8   <- mkRippleCarryAdder8();
    RCA8_ifc  rca8_1   <- mkRippleCarryAdder8();
    RCA8_ifc  rca8_2   <- mkRippleCarryAdder8();
    // RCA9_ifc  rca9   <- mkRippleCarryAdder9();
    // RCA9_ifc  rca9_1 <- mkRippleCarryAdder9();

    RCA24_ifc rca24      <- mkRippleCarryAdder24();
    RCA24_ifc rca24_neg  <- mkRippleCarryAdder24();
    RCA48_ifc rca48  <- mkRippleCarryAdder48();

    DF1_ifc distOf1 <- mkDistOfOne();

    // function Int#(8) distOfLeadingZero(Bit#(48) a);
    //     Int#(8) dist1;
    //     dist1 = (a[47]==1'b1)? 0 : ((a[46]==1'b1)? 1 : ((a[45]==1'b1)? 2 : ((a[44]==1'b1)? 3 : ((a[43]==1'b1)? 4 : ((a[42]==1'b1)? 5 :((a[41]==1'b1)? 6 :((a[40]==1'b1)? 7 : ((a[39]==1'b1)? 8 : ((a[38]==1'b1)? 9 : ((a[37]==1'b1)? 10 : ((a[36]==1'b1)? 11 :((a[35]==1'b1)? 12 :((a[34]==1'b1)? 13 :((a[33]==1'b1)? 14 :((a[32]==1'b1)? 15 :((a[31]==1'b1)? 16 :((a[30]==1'b1)? 17 :((a[29]==1'b1)? 18 :((a[28]==1'b1)? 19 :((a[27]==1'b1)? 20 :((a[26]==1'b1)? 21 :((a[25]==1'b1)? 22 :((a[24]==1'b1)? 23 :((a[23]==1'b1)? 24 :((a[22]==1'b1)? 25 :((a[21]==1'b1)? 26 :((a[20]==1'b1)? 27 :((a[19]==1'b1)? 28 :((a[18]==1'b1)? 29 :(a[17]==1'b1)? 30 : ((a[16]==1'b1)? 31 : ((a[15]==1'b1)? 32 : ((a[14]==1'b1)? 33 :((a[13]==1'b1)? 34 :((a[12]==1'b1)? 35 : ((a[11]==1'b1)? 36 : ((a[10]==1'b1)? 37 : (a[9]==1'b1)? 38 :((a[8]==1'b1)? 39 : ((a[7]==1'b1)? 40 : (a[6]==1'b1)? 41 :((a[5]==1'b1)? 42 : ((a[4]==1'b1)? 43 : ((a[3]==1'b1)? 44 :((a[2]==1'b1)? 45 : ((a[1]==1'b1)? 46 :((a[0]==1'b1)? 47 : 48 ) )) ))) )) ))) ) ))) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ))))) ) )))));
    //     return dist1;
    // endfunction : distOfLeadingZero



    
    function Bit#(48) barrelShifterR(Bit#(48) temp,Bit#(8) shift_num);
        temp = ((shift_num[7]==1'b1) ? (temp >> 128) : temp);  // Shift by 128 if bit 7 of shift_num is set
        temp = ((shift_num[6]==1'b1) ? (temp >> 64)  : temp);  // Shift by 64 if bit 6 is set
        temp = ((shift_num[5]==1'b1) ? (temp >> 32)  : temp);  // Shift by 32 if bit 5 is set
        temp = ((shift_num[4]==1'b1) ? (temp >> 16)  : temp);  // Shift by 16 if bit 4 is set
        temp = ((shift_num[3]==1'b1) ? (temp >> 8)   : temp);  // Shift by 8 if bit 3 is set
        temp = ((shift_num[2]==1'b1) ? (temp >> 4)   : temp);  // Shift by 4 if bit 2 is set
        temp = ((shift_num[1]==1'b1) ? (temp >> 2)   : temp);  // Shift by 2 if bit 1 is set
        temp = ((shift_num[0]==1'b1) ? (temp >> 1)   : temp);  // Shift by 1 if bit 0 is set
        return temp;
    endfunction : barrelShifterR

    function Bit#(48) barrelShifterL(Bit#(48) temp,Bit#(8) shift_num);
        temp = ((shift_num[7]==1'b1) ? (temp << 128) : temp);  // Shift by 128 if bit 7 of shift_num is set
        temp = ((shift_num[6]==1'b1) ? (temp << 64)  : temp);  // Shift by 64 if bit 6 is set
        temp = ((shift_num[5]==1'b1) ? (temp << 32)  : temp);  // Shift by 32 if bit 5 is set
        temp = ((shift_num[4]==1'b1) ? (temp << 16)  : temp);  // Shift by 16 if bit 4 is set
        temp = ((shift_num[3]==1'b1) ? (temp << 8)   : temp);  // Shift by 8 if bit 3 is set
        temp = ((shift_num[2]==1'b1) ? (temp << 4)   : temp);  // Shift by 4 if bit 2 is set
        temp = ((shift_num[1]==1'b1) ? (temp << 2)   : temp);  // Shift by 2 if bit 1 is set
        temp = ((shift_num[0]==1'b1) ? (temp << 1)   : temp);  // Shift by 1 if bit 0 is set
        return temp;
    endfunction : barrelShifterL



    rule add_rl((rg_inp_valid)&&(!rg_add1_valid)&&(!rg_add2_valid));
        Bit#(32) d = rg_inpA;
        Bit#(32) e = rg_inpB;
        Bit#(8) exp1 = unpack(d[30:23]);
        Bit#(8) exp2 = unpack(e[30:23]);
        Bit#(32) max='b0;
        Bit#(32) min='b0;
        Bit#(1) sign_max=1'b1;
        Bit#(1) sign_min=1'b1;

        Bit#(24) final_mant = 'b0;
        Bit#(8)  exp_final  = 'b0;
        Bit#(1)  sign_final = 1'b0;
        Bool finish = False;
        Bit#(8) final_exp = 'b0;
        Bit#(8) shift_num = 'b0;
        Bit#(48) manD='b0;
        Bit#(48) manE='b0;

        // Bool add_overflow = False;
        // Bool exp_overflow = False;

        

        Bit#(2) indicator = 2'b00;
        if(exp1>exp2) 
        begin
            indicator = 2'b01;
            max = d;
            min = e;
            sign_max = d[31];
            sign_min = e[31];
        end 
        else if(exp1==exp2) 
        begin
            indicator = 2'b10;
            if(d[22:0] > e[22:0]) 
            begin
                max = d;
                min = e;
                sign_max = d[31];
                sign_min = e[31];
            end 
            else if(d[22:0] < e[22:0]) 
            begin
                max = e;
                min = d;
                sign_max = e[31];
                sign_min = d[31];
            end 
            else 
            begin
                final_mant = 'b0;
                exp_final  = 'b0;
                sign_final =1'b0;
                finish = True;
            end
        end 
        else 
        begin
            indicator = 2'b11;
            max = e;
            min = d;
            sign_max = e[31];
            sign_min = d[31];
        end
        
        rg_indicator <= indicator;
        final_exp = max[30:23];

        // Barrel Shifter Implementation
        let res_diff_exp = rca8.result(max[30:23],~min[30:23],1'b1);
        shift_num = (res_diff_exp.sum<pack(8'd48))?res_diff_exp.sum : pack(8'd48);
        manD[47] = 1'b1;
        manE[47] = 1'b1;
        manD[46:24] = max[22:0];
        manE[46:24] = min[22:0];
    

        Bit#(48) temp='b0;
        // temp = manE;
        // temp = ((shift_num[7]==1'b1) ? (temp >> 128) : temp);  // Shift by 128 if bit 7 of shift_num is set
        // temp = ((shift_num[6]==1'b1) ? (temp >> 64)  : temp);  // Shift by 64 if bit 6 is set
        // temp = ((shift_num[5]==1'b1) ? (temp >> 32)  : temp);  // Shift by 32 if bit 5 is set
        // temp = ((shift_num[4]==1'b1) ? (temp >> 16)  : temp);  // Shift by 16 if bit 4 is set
        // temp = ((shift_num[3]==1'b1) ? (temp >> 8)   : temp);  // Shift by 8 if bit 3 is set
        // temp = ((shift_num[2]==1'b1) ? (temp >> 4)   : temp);  // Shift by 4 if bit 2 is set
        // temp = ((shift_num[1]==1'b1) ? (temp >> 2)   : temp);  // Shift by 2 if bit 1 is set
        // temp = ((shift_num[0]==1'b1) ? (temp >> 1)   : temp);  // Shift by 1 if bit 0 is set
        // manE = temp;
        manE = barrelShifterR(manE,shift_num);
        
        

        // Addition operation
        
        Bit#(1) c_1   = 1'b0;
        Bool opp_sign = False;
        Bit#(48) manf_extended='b0;

        if((sign_max^sign_min) == 1'b1) begin
            manE = ~manE;
            c_1  = 1'b1;
            opp_sign = True;
        end
        rg_manD <= manD;
        rg_manE <= manE;

        let ans = rca48.result(manD,manE,c_1);

        rg_shift_num <= shift_num;
        
        distOf1.start(ans.sum);

        rg_add1_valid <= True;
        rg_addf_par <= Addf_par_data{
            sign_max : sign_max,
            opp_sign : opp_sign,
            finish   : finish,
            rca48ans : ans,
            manf_extended : manf_extended,
            manD          : manD,
            manE          : manE,
            final_mant    :final_mant,
            exp_final     : exp_final,
            final_exp     : final_exp
        };
        
    endrule : add_rl

    rule add2_rl((rg_inp_valid)&&(rg_add1_valid)&&(!rg_add2_valid));
        Int#(8) sub_counta = 0;
        Bit#(8) sub_val_bita = 'b0;

        Bit#(8) val1a  ='b0;
        Bit#(1) valca = 1'b0;
        
        Bit#(1) guard_bita=1'b0;
        Bit#(1) round_bita=1'b0;
        Bit#(1) sticky_bita = 1'b0;

        Bit#(1) carrya = 1'b0;
        Bit#(24) manfa ='b0;
        Bit#(1) add_2_expa = 1'b0;

        Bit#(1) sign_maxa  = 'b0;
        Bit#(24) final_manta = 'b0;
        Bit#(8) exp_finala     = 'b0;
        Bit#(8) final_expa = 'b0;
        Bool finisha = False;

        Bit#(1)  valc2 = 1'b0;
        Bit#(1)  val3 = 1'b0;
        Bit#(1)  valc3 = 1'b0;

        Bit#(48) manf_extendeda = 'b0;

        Bool exp_overflowa = False;
        if(isValid(distOf1.result()))
        begin
            let opp_signa      = rg_addf_par.opp_sign;
            let finisha       = rg_addf_par.finish;
            let rca48ansa      = rg_addf_par.rca48ans;
            let manf_extendeda = rg_addf_par.manf_extended;
            let manDa          = rg_addf_par.manD;
            let manEa          = rg_addf_par.manE;

            sign_maxa    = rg_addf_par.sign_max;
            final_manta  = rg_addf_par.final_mant;
            exp_finala   = rg_addf_par.exp_final;
            final_expa  = rg_addf_par.final_exp;
            
            manf_extendeda  = rca48ansa.sum;
            carrya = rca48ansa.overflow;

            
             
            if(opp_signa)
            begin
                // subtraction logic
                // sub_count = distOfLeadingZero(manf_extended);
                sub_counta = fromMaybe('d0,distOf1.result());
                sub_val_bita = pack(sub_counta);
                manf_extendeda = barrelShifterL(manf_extendeda,sub_val_bita);

                guard_bita = manf_extendeda[23];
                round_bita = manf_extendeda[22];
                
                val1a = ~sub_val_bita;
                valca = 1'b1;
            end else begin
                // Addition logic
                add_2_expa = rca48ansa.overflow;
                if(rca48ansa.overflow==1'b1) 
                begin
                    manf_extendeda[47]    = rca48ansa.overflow;
                    manf_extendeda[46:0] = rca48ansa.sum[47:1];
                    // guard_bita = manf_extendeda[24];
                    // round_bita = manf_extendeda[23];
                    // guard_bit = ans.sum[24];
                    // round_bit = ans.sum[23];
                    sticky_bita = rca48ansa.sum[0];
                end 
                val1a = 'b0;
                valca = add_2_expa;
            end
            manfa = manf_extendeda[47:24];
            rg_add_out_over <= manf_extendeda;
            let ap = (manf_extendeda[21:0] == 22'b0)? 1'b0 : 1'b1;
            sticky_bita = sticky_bita | ap;
            for(Integer i =21;i>=0;i=i-1) begin
                sticky_bita = sticky_bita|manf_extendeda[i];
            end

            guard_bita = manf_extendeda[23];
            round_bita = manf_extendeda[22];
            

            
            // Exponent addition
            // rg_indicator <= 2'b11;
            let rca8_ans = rca8_1.result(final_expa,val1a,valca);
            final_expa = rca8_ans.sum;
            if(rca8_ans.overflow==1'b1)begin
                exp_overflowa = True;
            end
            // rounding-off logic
            Bool incr = False;
            Bool decr = False;
            if(guard_bita==1'b1)begin
                if((round_bita==1'b1)||(sticky_bita==1'b1)) begin
                    incr = True;
                end else if((round_bita==1'b0)&&(sticky_bita==1'b0)) begin
                    if(manfa[0]==1'b0)begin
                        decr = True;
                    end else begin
                        incr = True;
                    end
                end
            end
            if(sign_maxa==1'b1) begin
                if(incr) begin
                    incr = False;
                    decr = True;
                end else if(decr) begin
                    decr = False;
                    incr = True;
                end
            end
            Bit#(24) val2  = 'b0;
            Bit#(1)  valc1 = 1'b0;


            if(incr) begin
                valc1 = 1'b1;
                val2  = 'b0;
            end 
            else if(decr) begin
                valc1 = 1'b0;
                val2  = 24'b111111111111111111111111;
            end

            
            // Bool one_seen = False;
            let rca24_ans = rca24.result(manfa,val2,valc1);
            manfa = rca24_ans.sum;

            if((incr)&&(rca24_ans.overflow==1'b1))
            begin
                manfa[23]   = rca24_ans.overflow;
                manfa[22:0] = rca24_ans.sum[23:1];
                valc2 = 1'b1;
            end 
            else if((decr)&&(rca24_ans.sum[23]==1'b0))
            begin
                // inverting the sign
                sign_maxa = ~sign_maxa;
                // issue2
                valc2 = 1'b0;
                manfa = ~manfa;
                valc3 = 1'b0;
                // manf[23:1] = rca24_ans.sum[23:1];
                // manf[0]    = 1'b1;
                // manf[0] = 1'b1;
            end 
            // else 
            // begin
            //     manf1 = rca24_ans.sum;
            // end
            let rca24_ans1 = rca24_neg.result(manfa,'b0,valc3);
            let rca8_ans1 = rca8_2.result(final_expa,'b0,valc2);
            final_expa = rca8_ans1.sum;

            manfa = rca24_ans1.sum;
            if(rca8_ans1.overflow==1'b1) begin
                exp_overflowa = True;
            end
            if(!finisha) begin
                final_manta = manfa;
                exp_finala = final_expa;
            end
            Bit#(32) fp_add_val='b0;
            fp_add_val[31]    = sign_maxa;
            fp_add_val[30:23] = exp_finala;
            fp_add_val[22:0]  = final_manta[22:0];

            rg_add_out_overflow <= exp_overflowa;
            // rg_shift_num <= shift_num;
            rg_sub_count <= sub_counta;
            rg_add_out   <= fp_add_val;
            rg_add_out_valid <= True;
            rg_add2_valid <= True;
            rg_add1_valid <= False;
            rg_inp_valid <= False;
        end

    endrule : add2_rl

    method Action start(Bit#(32) a, Bit#(32) b);
        rg_inpA      <= a;
        rg_inpB      <= b;
        rg_inp_valid <= True;
    endmethod : start

    method Maybe#(Adderfp32_result) result();
        Maybe#(Adderfp32_result) x = tagged Invalid;
        if(rg_add2_valid) begin
            x = tagged Valid Adderfp32_result{
                val          : rg_add_out,
                manD         : rg_manD,
                manE         : rg_manE,
                add_inter    : rg_add_out_over,
                indicator    : rg_indicator,
                exp_overflow : rg_add_out_overflow,
                shift_num    : rg_shift_num,
                sub_count    : rg_sub_count
            };
        end
        return x;
    endmethod : result

endmodule : mkAdder_fp32



(*synthesize*)
module mkMAC_fp32(MACf32_ifc);
    // Registers used
    Reg#(Bit#(16)) rg_inpA      <- mkReg(0);
    Reg#(Bit#(16)) rg_inpB      <- mkReg(0);
    Reg#(Bit#(32)) rg_inpC      <- mkReg(0);
    Reg#(Bool)     rg_inp_valid  <- mkReg(False);
    Reg#(Bit#(16)) rg_product   <- mkReg(0);
    // Reg#(Bit#(1))  rg_sign      <- mkReg(0);
    // Reg#(Bit#(8))  rg_exponent  <- mkReg(0);
    // Reg#(Bit#(2))   rg_indicator <- mkReg(0);

    Reg#(Bit#(16)) rg_mul_out          <- mkReg(0);
    Reg#(Bool)     rg_mul_valid        <- mkReg(False);
    Reg#(Bool)     rg_mul_out_overflow <- mkReg(False);

    Reg#(Bit#(32)) rg_add_out          <- mkReg(0);
    Reg#(Bool)     rg_add_valid        <- mkReg(False);
    Reg#(Bool)     rg_add_out_overflow <- mkDReg(False);
    Reg#(Bool)     rg_add_out_valid <- mkDReg(False);


    // Reg#(Bit#(48)) rg_manD <- mkReg(0);
    // Reg#(Bit#(48)) rg_manE <- mkReg(0);
    Reg#(Bit#(48)) rg_add_out_over <- mkReg(0);
    // Reg#(Int#(8))  rg_sub_count <-mkReg(unpack(0));


    // Reg#(Addf_par_data) rg_addf_par <- mkReg(unpack(0));
    // Reg#(Bit#(8)) rg_shift_num <- mkReg(0);


    // Module instantiation
    AA7_ifc aa7[7];
    for(Integer i = 0;i < 7;i = i+1) begin
        aa7[i] <- mkArrayAdder7();
    end

    RCA7_ifc  rca7   <- mkRippleCarryAdder7();
    RCA8_ifc  rca8_3   <- mkRippleCarryAdder8();
    RCA8_ifc  rca8_4   <- mkRippleCarryAdder8();
    RCA8_ifc  rca8_neg   <- mkRippleCarryAdder8();
    RCA9_ifc  rca9   <- mkRippleCarryAdder9();
    RCA9_ifc  rca9_1 <- mkRippleCarryAdder9();

    // Instantiating the Floating point Adder module
    Adderfp32_ifc addf32_1 <- mkAdder_fp32();


    rule mul_rl((rg_inp_valid)&&(!rg_mul_valid)&&(!rg_add_valid));
        Bool mult_overflow = False;
        Bit#(8) manA ='b0; 
        Bit#(8) manB ='b0;
        Bit#(8) expA = rg_inpA[14:7];
        Bit#(8) expB = rg_inpB[14:7];
        
        manA[7] = 1'b1;
        manB[7] = 1'b1;
        manA[6:0] = rg_inpA[6:0];
        manB[6:0] = rg_inpB[6:0];

        Bit#(7)   sum_par[7] ;
        Bit#(7) carry_par[7] ;


        Bit#(16) mul_result = 16'b0;

        AA7_result res[7];

        Bit#(7) a_1='b0;
        Bit#(7) b_1='b0;
        Bit#(7) cin_1='b0;
        Bit#(7) a_2='b0;
        Bit#(7) b_2='b0;
        Bit#(7) a_3='b0;
        Bit#(7) b_3='b0;
        Bit#(7) a_4='b0;
        Bit#(7) b_4='b0;
        Bit#(7) a_5='b0;
        Bit#(7) b_5='b0;
        Bit#(7) a_6='b0;
        Bit#(7) b_6='b0;
        Bit#(7) a_7='b0;
        Bit#(7) b_7='b0;
        Bit#(7) a_8='b0;
        Bit#(7) b_8='b0;
        

        

        a_1 = signExtend(manA[7:1] & signExtend(manB[0]));
        b_1 = signExtend(manA[6:0] & signExtend(manB[1]));
        cin_1[6:1] = signExtend(manA[5:0] & signExtend(manB[2]));
        cin_1[0] = 1'b0;
        mul_result[0] = manA[0] & manB[0];

        res[0] = aa7[0].result(a_1,b_1,cin_1);
        sum_par[0] = res[0].sum;
        carry_par[0] = res[0].carryOut;
        mul_result[1] = sum_par[0][0];

        a_2[6] = manA[7] & manB[1];
        a_2[5:0] = sum_par[0][6:1];
        b_2[6] = manA[6] & manB[2];
        b_2[5:1] = manA[4:0] & signExtend(manB[3]);
        b_2[0]   = 1'b0;

        res[1] = aa7[1].result(a_2,b_2,carry_par[0]);
        sum_par[1] = res[1].sum;
        carry_par[1] = res[1].carryOut;
        mul_result[2] = sum_par[1][0];

        a_3[6] = manA[7] & manB[2];
        a_3[5:0] = sum_par[1][6:1];
        b_3[6] = manA[6] & manB[3];
        b_3[5] = manA[5] & manB[3];
        b_3[4:1] = manA[3:0] & signExtend(manB[4]);
        b_3[0]   = 1'b0;

        
        res[2] = aa7[2].result(a_3,b_3,carry_par[1]);
        sum_par[2] = res[2].sum;
        carry_par[2] = res[2].carryOut;
        mul_result[3] = sum_par[2][0];

        a_4[6] = manA[7] & manB[3];
        a_4[5:0] = sum_par[2][6:1];
        b_4[6] = manA[6] & manB[4];
        b_4[5] = manA[5] & manB[4];
        b_4[4] = manA[4] & manB[4];
        b_4[3:1] = manA[2:0] & signExtend(manB[5]);
        b_4[0]   = 1'b0;

        
        res[3] = aa7[3].result(a_4,b_4,carry_par[2]);
        sum_par[3] = res[3].sum;
        carry_par[3] = res[3].carryOut;
        mul_result[4] = sum_par[3][0];

        a_5[6] = manA[7] & manB[4];
        a_5[5:0] = sum_par[3][6:1];
        b_5[6] = manA[6] & manB[5];
        b_5[5] = manA[5] & manB[5];
        b_5[4] = manA[4] & manB[5];
        b_5[3] = manA[3] & manB[5];
        b_5[2:1] = manA[1:0] & signExtend(manB[6]);
        b_5[0]   = 1'b0;

        
        res[4] = aa7[4].result(a_5,b_5,carry_par[3]);
        sum_par[4] = res[4].sum;
        carry_par[4] = res[4].carryOut;
        mul_result[5] = sum_par[4][0];

        a_6[6] = manA[7] & manB[5];
        a_6[5:0] = sum_par[4][6:1];
        b_6[6] = manA[6] & manB[6];
        b_6[5] = manA[5] & manB[6];
        b_6[4] = manA[4] & manB[6];
        b_6[3] = manA[3] & manB[6];
        b_6[2] = manA[2] & manB[6];
        b_6[1] = manA[0] & manB[7];
        b_6[0]   = 1'b0;

        
        res[5] = aa7[5].result(a_6,b_6,carry_par[4]);
        sum_par[5] = res[5].sum;
        carry_par[5] = res[5].carryOut;
        mul_result[6] = sum_par[5][0];

        a_7[6] = manA[7] & manB[6];
        a_7[5:0] = sum_par[5][6:1];
        b_7[6] = manA[6] & manB[7];
        b_7[5] = manA[5] & manB[7];
        b_7[4] = manA[4] & manB[7];
        b_7[3] = manA[3] & manB[7];
        b_7[2] = manA[2] & manB[7];
        b_7[1] = manA[1] & manB[7];
        b_7[0]   = 1'b0;

        
        res[6] = aa7[6].result(a_7,b_7,carry_par[5]);
        sum_par[6] = res[6].sum;
        carry_par[6] = res[6].carryOut;
        mul_result[7] = sum_par[6][0];


        a_8[6] = manA[7] & manB[7];
        a_8[5:0] = sum_par[6][6:1];

        b_8 = carry_par[6];
            
        let rca7_res     = rca7.result(a_8,b_8,1'b0);

        mul_result[14:8]  = rca7_res.sum;
        mul_result[15]    = rca7_res.overflow;
        rg_product       <= mul_result;
        
        Bit#(9) ap = 'b0;
        Bit#(9) bp = 'b0;
        ap[8] = 1'b0;
        bp[8] = 1'b0;
        ap[7:0] = expA;
        bp[7:0] = expB;

        let add_exp_res = rca9.result(ap,bp,mul_result[15]);
        Bit#(9) expC = add_exp_res.sum;
        Bit#(9) constant127 = 9'b110000001;

        let add_exp_res_1 = rca9_1.result(expC,constant127,1'b0);
        let c1 = add_exp_res_1.sum;

        // rg_exponent <= c1[7:0];

        // overflow logic need to discuss with mouri
        if(add_exp_res_1.sum[8]==1'b1) begin
            c1 = 'b1;
            mult_overflow = True;
        end

        Bit#(1) sign = rg_inpA[15]^rg_inpB[15];
        Bit#(32) fp_mul_res='b0;

        fp_mul_res[31] = sign;
        fp_mul_res[30:23] = c1[7:0];

        if(mul_result[15]==1'b0) begin
            fp_mul_res[22:9] = mul_result[13:0];
            fp_mul_res[8:0]  = 9'b0;
        end else begin
            fp_mul_res[22:8] = mul_result[14:0];
            fp_mul_res[7:0]  = 8'b0;
        end
        
        // Round Off Logic
        Bit#(1) guard_bita = fp_mul_res[15];
        Bit#(1) round_bita = fp_mul_res[14];
        Bit#(1) sticky_bita = 1'b0;

        for(Integer i=0;i<=13;i=i+1) begin
            sticky_bita = sticky_bita|fp_mul_res[i];
        end

        Bit#(8) manfa = {1'b1,fp_mul_res[22:16]};
        Bit#(8) final_expa = fp_mul_res[30:23];
        Bool incr = False;
        Bool decr = False;
        if(guard_bita==1'b1)begin
            if((round_bita==1'b1)||(sticky_bita==1'b1)) begin
                incr = True;
            end else if((round_bita==1'b0)&&(sticky_bita==1'b0)) begin
                if(manfa[0]==1'b0)begin
                    decr = True;
                end else begin
                    incr = True;
                end
            end
        end
        Bit#(1) sign_max = fp_mul_res[31];
        if(sign_max==1'b1) begin
            if(incr) begin
                incr = False;
                decr = True;
            end else if(decr) begin
                decr = False;
                incr = True;
            end
        end
        Bit#(8) val2 = 'b0;
        Bit#(1)  valc1 = 1'b0;

        if(incr) begin
            valc1 = 1'b1;
            val2  = 'b0;
        end else if (decr) begin
            valc1 = 1'b0;
            val2  = 8'b11111111;
        end

        // Bool one_seen = False;
        let rca8_ans3 = rca8_3.result(manfa,val2,valc1);
        manfa = rca8_ans3.sum;

        Bit#(1) valc2 = 1'b0;
        Bit#(1) valc3 = 1'b0;

        if((incr)&&(rca8_ans3.overflow==1'b1))
        begin
            manfa[7]   = rca8_ans3.overflow;
            manfa[6:0] = rca8_ans3.sum[7:1];
            valc2 = 1'b1;
        end 
        else if((decr)&&(rca8_ans3.sum[7]==1'b0))
        begin
            // inverting the sign
            sign_max = ~sign_max;
            // issue2
            valc2 = 1'b0;
            manfa = ~manfa;
            valc3 = 1'b0;
            // manf[23:1] = rca24_ans.sum[23:1];
            // manf[0]    = 1'b1;
            // manf[0] = 1'b1;
        end 
        // else 
        // begin
        //     manf1 = rca24_ans.sum;
        // end
        let rca8_ansneg = rca8_neg.result(manfa,'b0,valc3);
        let rca8_ans6 = rca8_4.result(final_expa,'b0,valc2);
        final_expa = rca8_ans6.sum;

        manfa = rca8_ansneg.sum;
        if(rca8_ans6.overflow==1'b1) begin
            final_expa = 'b1;
            mult_overflow = True;
        end

        Bit#(16) fp_mul16_res = 'b0;
        fp_mul16_res[15] = sign_max;
        fp_mul16_res[14:7] = final_expa;
        fp_mul16_res[6:0]  = manfa[6:0];
        //
        rg_mul_out      <= fp_mul16_res;
        rg_mul_valid    <= True;
        rg_mul_out_overflow <= mult_overflow;

        Bit#(32) inpD = {fp_mul16_res,16'b0};
        Bit#(32) inpE = rg_inpC;
        addf32_1.start(inpD,inpE);
    endrule : mul_rl
    
    rule acc_rl((rg_inp_valid)&&(rg_mul_valid)&&(!rg_add_valid));
        Bit#(32) fp_add_result = 'b0;
        if(isValid(addf32_1.result())) begin
            let add_ans = fromMaybe(unpack(0),addf32_1.result());
            fp_add_result = add_ans.val;
            rg_add_out <= fp_add_result;
            rg_mul_valid <= False;
            rg_inp_valid <= False;
            rg_add_valid <= True;
        end
    endrule : acc_rl


    method Action start(Bit#(16) a, Bit#(16) b, Bit#(32) c);
        rg_inpA      <= a;
        rg_inpB      <= b;
        rg_inpC      <= c;
        rg_inp_valid <= True;
    endmethod : start

    method Bit#(32) result()if(rg_add_valid);
        // Maybe#(MACf32_result) x = tagged Invalid;
        // if(rg_add_valid) begin 
        //     x = tagged Valid MACf32_result{
        //         val      : rg_add_out,
        //         product  : rg_mul_out,
        //         add_overflow  : False,
        //         mult_overflow : False};
        // end
        return rg_add_out;
    endmethod : result
endmodule : mkMAC_fp32

endpackage : MAC_fp32
