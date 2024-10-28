package MAC_int32;
// importing packages
import DReg::*;

// Result structure for Ripple Carry Adder
typedef struct {
    Bit#(1) overflow;
    Bit#(32) sum;
} RCA32_result deriving(Bits, Eq);

typedef struct {
    Bit#(1) overflow;
    Bit#(9) sum;
} RCA9_result deriving(Bits, Eq);

// Result structure for Full Adder
typedef struct {
    Bit#(1) carryOut;
    Bit#(1) sum;
} FA_result deriving(Bits, Eq);

typedef struct {
    Bit#(8) carryOut;
    Bit#(8) sum;
} AA8_result deriving(Bits, Eq);

typedef struct {
    Bit#(16) product;
    Bit#(32) mac_val;
    Bool     overflow;
} MACI32_result deriving(Bits, Eq);

// Full Adder Interface
interface FA_ifc;
    method FA_result result(Bit#(1) a, Bit#(1) b, Bit#(1) cin);
endinterface : FA_ifc

// Ripple Carry Adder Interface
interface RCA32_ifc;
    method RCA32_result result(Bit#(32) a, Bit#(32) b, Bit#(1) cin);
endinterface : RCA32_ifc

interface RCA9_ifc;
    method RCA9_result result(Bit#(9) a, Bit#(9) b, Bit#(1) cin);
endinterface : RCA9_ifc

// Array Adders interfaces
interface AA8_ifc;
    method AA8_result result(Bit#(8) a, Bit#(8) b, Bit#(8) cin);
endinterface : AA8_ifc
// Int8 Multiplier Interface
interface MACI32_ifc;
    method Action start(Bit#(8) a, Bit#(8) b, Bit#(32) c);
    method Bit#(32) result();
endinterface : MACI32_ifc


module mkRippleCarryAdder9(RCA9_ifc);

    function FA_result fulladdition(Bit#(1) a, Bit#(1) b, Bit#(1) cin);
        FA_result res = FA_result{
            carryOut : (a & b) | (b & cin) | (cin & a),
            sum      : a ^ b ^ cin
        };
        return res;
    endfunction : fulladdition

    function RCA9_result rippleCarryAddition9(Bit#(9) a, Bit#(9) b, Bit#(1) cin);
        Bit#(9) sum;
        Bit#(10) carry = 10'b0;

        carry[0] = cin;
        for(Integer i = 0;i<9;i=i+1) begin
            FA_result r = fulladdition(a[i],b[i],carry[i]);
            sum[i]     = r.sum;
            carry[i+1] = r.carryOut;
        end
        RCA9_result rca9_res = RCA9_result{
            overflow : carry[9],
            sum      : sum
        };
        // rca9_res.sum = sum;
        // rca9_res.overflow = carry[9];
        return rca9_res;
    endfunction : rippleCarryAddition9

    method RCA9_result result(Bit#(9) a, Bit#(9) b, Bit#(1) cin);
        return rippleCarryAddition9(a,b,cin);
    endmethod : result
    
endmodule : mkRippleCarryAdder9

module mkRippleCarryAdder32(RCA32_ifc);
    function RCA32_result rippleCarryAddition32 (Bit#(32) a,Bit#(32) b,Bit#(1) cin);
        Bit#(32) sum;
        Bit#(33) carry='b0;
        carry[0] = cin;

        for(Integer i = 0;i<32;i=i+1) begin
            sum[i]     = (a[i]^b[i]^carry[i]);
            carry[i+1] = (a[i]&b[i])|(b[i]&carry[i])|(carry[i]&a[i]);
        end
        RCA32_result rca32_res;
        rca32_res.sum = sum;
        rca32_res.overflow = carry[32];
        return rca32_res;
    endfunction : rippleCarryAddition32
    method RCA32_result result(Bit#(32) a, Bit#(32) b, Bit#(1) cin);
        return rippleCarryAddition32(a,b,cin);
    endmethod : result
endmodule : mkRippleCarryAdder32

module mkArrayAdder8(AA8_ifc);
    function FA_result fulladdition(Bit#(1) a, Bit#(1) b, Bit#(1) cin);
        FA_result res = FA_result{
            carryOut : (a & b) | (b & cin) | (cin & a),
            sum      : a ^ b ^ cin
        };
        return res;
    endfunction : fulladdition
    function AA8_result arrayAddition8(Bit#(8) a, Bit#(8) b, Bit#(8) cin);
        Bit#(8) sum;
        Bit#(8) carry;
        for(Integer i = 0;i<8;i=i+1) begin
            FA_result r = fulladdition(a[i],b[i],cin[i]);
            sum[i]   = r.sum;
            carry[i] = r.carryOut;
        end
        AA8_result res = AA8_result{
            carryOut : carry,
            sum      : sum
        };
        return res;
    endfunction : arrayAddition8
    method AA8_result result(Bit#(8) a, Bit#(8) b, Bit#(8) cin);
        return arrayAddition8( a, b, cin);
    endmethod : result
endmodule : mkArrayAdder8

(*synthesize*)
module mkMAC_int32(MACI32_ifc);
    // Registers used

    Reg#(Bit#(8)) rg_inpA      <- mkReg(0);
    Reg#(Bit#(8)) rg_inpB      <- mkReg(0);
    Reg#(Bit#(32)) rg_inpC      <- mkReg(0);
    // Reg#(Bit#(16)) rg_inpC_1     <- mkReg(0);
    Reg#(Bool)    rg_inp_valid <- mkDReg(False);

    Reg#(Bit#(16)) rg_product       <- mkReg(0);
    // Reg#(Bit#(16)) rg_product1       <- mkReg(0);
    // Reg#(Bool)     rg_product_valid <- mkDReg(False);

    Reg#(Bit#(32)) rg_out       <- mkReg(0);
    Reg#(Bool)     rg_out_valid <- mkDReg(False);

    Reg#(Bool)     rg_out_overflow <- mkDReg(False);



    AA8_ifc aa8[6];
    for(Integer i = 0;i < 6;i = i+1) begin
        aa8[i] <- mkArrayAdder8();
    end

    RCA9_ifc  rca9 <- mkRippleCarryAdder9();
    RCA32_ifc  rca32 <- mkRippleCarryAdder32();

    rule mul_rl(rg_inp_valid);
        Bit#(15)   sum_par[6];
        Bit#(15) carry_par[6];
        Bit#(16) mul_result = 16'b0;


        Bit#(8) a_1='b0;
        Bit#(8) b_1='b0;
        Bit#(8) cin_1='b0;

        Bit#(8) a_2='b0;
        Bit#(8) b_2='b0;
        Bit#(8) cin_2='b0;

        Bit#(8) a_3='b0;
        Bit#(8) b_3='b0;
        Bit#(8) cin_3='b0;

        Bit#(8) a_4='b0;
        Bit#(8) b_4='b0;
        Bit#(8) cin_4='b0;

        Bit#(8) a_5='b0;
        Bit#(8) b_5='b0;
        Bit#(8) cin_5='b0;

        Bit#(8) a_6='b0;
        Bit#(8) b_6='b0;
        Bit#(8) cin_6='b0;

        Bit#(9) a_7='b0;
        Bit#(9) b_7='b0;


        mul_result[0] = rg_inpA[0] & rg_inpB[0];

        // Bit#(16) a0    = signExtend(rg_inpA & signExtend(rg_inpB[0]));
        // Bit#(15) a1    = signExtend(rg_inpA & signExtend(rg_inpB[1]));
        // Bit#(14) a2    = signExtend(rg_inpA & signExtend(rg_inpB[2]));
        
        cin_1[0]   = 1'b0;
        cin_1[7:1] = rg_inpA[6:0] & signExtend(rg_inpB[2]);

        a_1[7]     = 1'b1;
        a_1[6:0]   = signExtend(rg_inpA[7:1] & signExtend(rg_inpB[0]));
        a_1[6]     = ~a_1[6];
        
        b_1        = rg_inpA & signExtend(rg_inpB[1]);
        b_1[7]     = ~b_1[7];

        let res1       = aa8[0].result(a_1,b_1,cin_1);
        mul_result[1]  = res1.sum[0];

        
        a_2[6:0]       = res1.sum[7:1];
        a_2[7]         = ~(rg_inpA[7]&rg_inpB[2]);
        b_2[0]         = 1'b0;
        b_2[7:1]       = rg_inpA[6:0] & signExtend(rg_inpB[3]);
        cin_2          = res1.carryOut;
        let res2       = aa8[1].result(a_2,b_2,cin_2);
        mul_result[2]  = res2.sum[0];

        
        a_3[6:0]       = res2.sum[7:1];
        a_3[7]         = ~(rg_inpA[7]&rg_inpB[3]);
        b_3[0]         = 1'b0;
        b_3[7:1]       = rg_inpA[6:0] & signExtend(rg_inpB[4]);
        cin_3          = res2.carryOut;
        let res3       = aa8[2].result(a_3,b_3,cin_3);
        mul_result[3]  = res3.sum[0];

        
        a_4[6:0]       = res3.sum[7:1];
        a_4[7]         = ~(rg_inpA[7]&rg_inpB[4]);
        b_4[0]         = 1'b0;
        b_4[7:1]       = rg_inpA[6:0] & signExtend(rg_inpB[5]);
        cin_4          = res3.carryOut;
        let res4       = aa8[3].result(a_4,b_4,cin_4);
        mul_result[4]  = res4.sum[0];

        a_5[6:0]       = res4.sum[7:1];
        a_5[7]         = ~(rg_inpA[7]&rg_inpB[5]);
        b_5[0]         = 1'b0;
        b_5[7:1]       = rg_inpA[6:0] & signExtend(rg_inpB[6]);
        cin_5          = res4.carryOut;
        let res5       = aa8[4].result(a_5,b_5,cin_5);
        mul_result[5]  = res5.sum[0];

        a_6[6:0]       = res5.sum[7:1];
        a_6[7]         = ~(rg_inpA[7]&rg_inpB[6]);
        b_6[0]         = 1'b0;
        b_6[7:1]       = ~(rg_inpA[6:0] & signExtend(rg_inpB[7]));
        cin_6          = res5.carryOut;
        let res6       = aa8[5].result(a_6,b_6,cin_6);
        mul_result[6]  = res6.sum[0];

        a_7[6:0]       = res6.sum[7:1];
        a_7[7]         = rg_inpA[7]&rg_inpB[7];
        a_7[8]         = 1'b0;
        b_7[8]         = 1'b1;
        b_7[7:0]       = res6.carryOut[7:0];

        let rca9_res1     = rca9.result(a_7,b_7,1'b0);
        mul_result[15:7]  = rca9_res1.sum;

        rg_product           <= mul_result;
        
        Bit#(32) ab = signExtend(mul_result);
        Bit#(32) c   = rg_inpC;

        let res_adder = rca32.result(ab,c,1'b0);

        rg_out <= res_adder.sum;
        rg_out_valid <= True;

        if((ab[31]==c[31])&&(ab[31]!=res_adder.sum[31])) begin
            rg_out_overflow <= True;
        end
    endrule : mul_rl


    method Action start(Bit#(8) a, Bit#(8) b, Bit#(32) c);
        rg_inpA      <= a;
        rg_inpB      <= b;
        rg_inpC      <= c;
        rg_inp_valid <= True;
        
        // rg_out_valid <= False;
    endmethod : start

    method result() if(rg_out_valid);
        // let x = MACI32_result{
        //     product  : rg_product,
        //     mac_val  : rg_out,
        //     overflow : rg_out_overflow
        // };
        return rg_out;
    endmethod : result
endmodule : mkMAC_int32

endpackage : MAC_int32