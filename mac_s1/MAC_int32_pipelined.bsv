package MAC_int32_pipelined;
// importing packages
import DReg::*;
import FIFO::*;
import SpecialFIFOs::*;

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
    Bit#(1) overflow;
    Bit#(25) sum;
} RCA25_result deriving(Bits, Eq);

typedef struct {
    Bit#(16) product;
    Bit#(32) mac_val;
    Bool     overflow;
} MACI32_result deriving(Bits, Eq);

typedef struct {
    Bit#(8) a_inp;
    Bit#(8) b_inp;
    Bit#(32) c_inp;
} MAC_input deriving(Bits, Eq);

typedef struct {
    Bit#(32) result;
    Bool overflow;
} MAC_op deriving(Bits, Eq);

typedef struct {
    Bit#(8) a;
    Bit#(8) b;
    Bit#(32) c;
    Bit#(8) sum_par;
    Bit#(8) carry_par;
    Bit#(16) prod;
    Bit#(1) rpa_carry;
    Bit#(32) rpa_sum;
} MAC_interm deriving(Bits, Eq);

typedef struct {
    Bit#(32) c;
    Bit#(32) prod;
    Bit#(1) rpa_carry;
    Bit#(32) rpa_sum;
} MAC_interm2 deriving(Bits, Eq);

typedef struct {
    Bit#(8) a;
    Bit#(8) b;
    Bit#(32) c;
    Bit#(9) sum_par9;
    Bit#(16) prod;
    Bit#(1) rpa_carry;
    Bit#(32) rpa_sum;
} MAC_interm9 deriving(Bits, Eq);

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



interface RCA25_ifc;
    method RCA25_result result(Bit#(25) a, Bit#(25) b, Bit#(1) cin);
endinterface : RCA25_ifc


// Array Adders interfaces
interface AA8_ifc;
    method AA8_result result(Bit#(8) a, Bit#(8) b, Bit#(8) cin);
endinterface : AA8_ifc
// Int8 Multiplier Interface
interface MACI32_ifc;
    method Action start(Bit#(8) a, Bit#(8) b, Bit#(32) c);
    method ActionValue#(Bit#(32)) result();
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

// module mkRippleCarryAdder16(RCA16_ifc);
//     function RCA16_result rippleCarryAddition16 (Bit#(16) a,Bit#(16) b,Bit#(1) cin);
//         Bit#(16) sum;
//         Bit#(17) carry='b0;
//         carry[0] = cin;

//         for(Integer i = 0;i<16;i=i+1) begin
//             sum[i]     = (a[i]^b[i]^carry[i]);
//             carry[i+1] = (a[i]&b[i])|(b[i]&carry[i])|(carry[i]&a[i]);
//         end
//         RCA16_result rca16_res;
//         rca16_res.sum = sum;
//         rca16_res.overflow = carry[16];
//         return rca16_res;
//     endfunction : rippleCarryAddition16
//     method RCA16_result result(Bit#(16) a, Bit#(16) b, Bit#(1) cin);
//         return rippleCarryAddition16(a,b,cin);
//     endmethod : result
// endmodule : mkRippleCarryAdder16


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
module mkMAC_int32_pipelined(MACI32_ifc);
    // FIFOs used in design
    FIFO#(MAC_input)  mac_inp_fifo <- mkPipelineFIFO();
    FIFO#(MAC_op)     mac_op_fifo <- mkPipelineFIFO();

    FIFO#(MAC_interm) mac_st1_fifo <- mkPipelineFIFO();
    FIFO#(MAC_interm) mac_st2_fifo <- mkPipelineFIFO();
    FIFO#(MAC_interm) mac_st3_fifo <- mkPipelineFIFO();
    FIFO#(MAC_interm) mac_st4_fifo <- mkPipelineFIFO();
    FIFO#(MAC_interm) mac_st5_fifo <- mkPipelineFIFO();
    FIFO#(MAC_interm) mac_st6_fifo <- mkPipelineFIFO();
    FIFO#(MAC_interm9) mac_st7_fifo <- mkPipelineFIFO();
    FIFO#(MAC_interm) mac_st8_fifo <- mkPipelineFIFO();
    FIFO#(MAC_interm2) mac_st9_fifo <- mkPipelineFIFO();

    


    function FA_result fulladdition(Bit#(1) a, Bit#(1) b, Bit#(1) cin);
        FA_result res = FA_result{
            carryOut : (a & b) | (b & cin) | (cin & a),
            sum      : a ^ b ^ cin
        };
        return res;
    endfunction : fulladdition
    AA8_ifc aa8[6];
    for(Integer i = 0;i < 6;i = i+1) begin
        aa8[i] <- mkArrayAdder8();
    end

    // RCA9_ifc  rca9 <- mkRippleCarryAdder9();
    RCA32_ifc  rca32 <- mkRippleCarryAdder32();
    RCA25_ifc  rca25 <- mkRippleCarryAdder25();
    RCA9_ifc  rca9   <- mkRippleCarryAdder9();
    

    rule stage_1_rl;
        Bit#(8) rg_inpA = 'b0;
        Bit#(8) rg_inpB = 'b0;
        Bit#(32) rg_inpC = 'b0;
        Bit#(32) sumf   = 'b0;
        MAC_input inp = mac_inp_fifo.first();

        rg_inpA = inp.a_inp;
        rg_inpB = inp.b_inp;
        rg_inpC = inp.c_inp;

        Bit#(16) mul_result = 'b0;

        Bit#(8) a_1='b0;
        Bit#(8) b_1='b0;
        Bit#(8) cin_1='b0;

        

        mul_result[0] = rg_inpA[0] & rg_inpB[0];
        cin_1[0]   = 1'b0;
        cin_1[7:1] = rg_inpA[6:0] & signExtend(rg_inpB[2]);
        a_1[7]     = 1'b1;
        a_1[6:0]   = signExtend(rg_inpA[7:1] & signExtend(rg_inpB[0]));
        a_1[6]     = ~a_1[6];
        b_1        = rg_inpA & signExtend(rg_inpB[1]);
        b_1[7]     = ~b_1[7];

        let res1       = aa8[0].result(a_1,b_1,cin_1);
        mul_result[1]  = res1.sum[0];


        let y = fulladdition(rg_inpC[0],mul_result[0],1'b0);
        sumf[0] = y.sum;
        MAC_interm out;
        out.a = rg_inpA;
        out.b = rg_inpB;
        out.c = rg_inpC;
        out.prod      = mul_result;
        out.sum_par   = res1.sum;
        out.carry_par = res1.carryOut;
        out.rpa_sum   = sumf;
        out.rpa_carry = y.carryOut;

        mac_st1_fifo.enq(out);
        mac_inp_fifo.deq();
    endrule : stage_1_rl

    rule stage_2_rl;
        Bit#(8) rg_inpA = 'b0;
        Bit#(8) rg_inpB = 'b0;
        Bit#(32) rg_inpC = 'b0;

        MAC_interm inp = mac_st1_fifo.first();

        rg_inpA = inp.a;
        rg_inpB = inp.b;
        rg_inpC = inp.c;
        Bit#(16) mul_result = inp.prod;
        Bit#(32) sumf      = inp.rpa_sum;

        Bit#(8) a_2='b0;
        Bit#(8) b_2='b0;
        Bit#(8) cin_2='b0;

        a_2[6:0]       = inp.sum_par[7:1];
        a_2[7]         = ~(rg_inpA[7]&rg_inpB[2]);
        b_2[0]         = 1'b0;
        b_2[7:1]       = rg_inpA[6:0] & signExtend(rg_inpB[3]);
        cin_2          = inp.carry_par;
        let res2       = aa8[1].result(a_2,b_2,cin_2);
        mul_result[2]  = res2.sum[0];

        

        let y = fulladdition(rg_inpC[1],mul_result[1],inp.rpa_carry);
        sumf[1] = y.sum;


        MAC_interm out;
        out.a = rg_inpA;
        out.b = rg_inpB;
        out.c = rg_inpC;
        out.prod      = mul_result;
        out.sum_par   = res2.sum;
        out.carry_par = res2.carryOut;
        out.rpa_sum   = sumf;
        out.rpa_carry = y.carryOut;

        mac_st2_fifo.enq(out);
        mac_st1_fifo.deq();
    endrule : stage_2_rl

    rule stage_3_rl;

        MAC_interm inp = mac_st2_fifo.first();
        Bit#(8) rg_inpA = 'b0;
        Bit#(8) rg_inpB = 'b0;
        Bit#(32) rg_inpC = 'b0;

        rg_inpA = inp.a;
        rg_inpB = inp.b;
        rg_inpC = inp.c;
        Bit#(16) mul_result = inp.prod;
        Bit#(32) sumf      = inp.rpa_sum;

        Bit#(8) a_3='b0;
        Bit#(8) b_3='b0;
        Bit#(8) cin_3='b0;

        a_3[6:0]       = inp.sum_par[7:1];
        a_3[7]         = ~(rg_inpA[7]&rg_inpB[3]);
        b_3[0]         = 1'b0;
        b_3[7:1]       = rg_inpA[6:0] & signExtend(rg_inpB[4]);
        cin_3          = inp.carry_par;
        let res3       = aa8[2].result(a_3,b_3,cin_3);
        mul_result[3]  = res3.sum[0];

        let y = fulladdition(rg_inpC[2],mul_result[2],inp.rpa_carry);
        sumf[2] = y.sum;


        MAC_interm out;
        out.a = rg_inpA;
        out.b = rg_inpB;
        out.c = rg_inpC;
        out.prod      = mul_result;
        out.sum_par   = res3.sum;
        out.carry_par = res3.carryOut;
        out.rpa_sum   = sumf;
        out.rpa_carry = y.carryOut;
        
        mac_st3_fifo.enq(out);
        mac_st2_fifo.deq();
    endrule : stage_3_rl

    rule stage_4_rl;

        MAC_interm inp = mac_st3_fifo.first();
        Bit#(8) rg_inpA = 'b0;
        Bit#(8) rg_inpB = 'b0;
        Bit#(32) rg_inpC = 'b0;

        rg_inpA = inp.a;
        rg_inpB = inp.b;
        rg_inpC = inp.c;
        Bit#(16) mul_result = inp.prod;
        Bit#(32) sumf      = inp.rpa_sum;

        Bit#(8) a_4='b0;
        Bit#(8) b_4='b0;
        Bit#(8) cin_4='b0;

        a_4[6:0]       = inp.sum_par[7:1];
        a_4[7]         = ~(rg_inpA[7]&rg_inpB[4]);
        b_4[0]         = 1'b0;
        b_4[7:1]       = rg_inpA[6:0] & signExtend(rg_inpB[5]);
        cin_4          = inp.carry_par;
        let res4       = aa8[3].result(a_4,b_4,cin_4);
        mul_result[4]  = res4.sum[0];

        let y = fulladdition(rg_inpC[3],mul_result[3],inp.rpa_carry);
        sumf[3] = y.sum;


        MAC_interm out;
        out.a = rg_inpA;
        out.b = rg_inpB;
        out.c = rg_inpC;
        out.prod      = mul_result;
        out.sum_par   = res4.sum;
        out.carry_par = res4.carryOut;
        out.rpa_sum   = sumf;
        out.rpa_carry = y.carryOut;
        
        mac_st4_fifo.enq(out);
        mac_st3_fifo.deq();
    endrule : stage_4_rl

    rule stage_5_rl;

        MAC_interm inp = mac_st4_fifo.first();
        Bit#(8) rg_inpA = 'b0;
        Bit#(8) rg_inpB = 'b0;
        Bit#(32) rg_inpC = 'b0;

        rg_inpA = inp.a;
        rg_inpB = inp.b;
        rg_inpC = inp.c;
        Bit#(16) mul_result = inp.prod;
        Bit#(32) sumf      = inp.rpa_sum;

        Bit#(8) a_5='b0;
        Bit#(8) b_5='b0;
        Bit#(8) cin_5='b0;

        a_5[6:0]       = inp.sum_par[7:1];
        a_5[7]         = ~(rg_inpA[7]&rg_inpB[5]);
        b_5[0]         = 1'b0;
        b_5[7:1]       = rg_inpA[6:0] & signExtend(rg_inpB[6]);
        cin_5          = inp.carry_par;
        let res5       = aa8[4].result(a_5,b_5,cin_5);
        mul_result[5]  = res5.sum[0];

        let y = fulladdition(rg_inpC[4],mul_result[4],inp.rpa_carry);
        sumf[4] = y.sum;


        MAC_interm out;
        out.a = rg_inpA;
        out.b = rg_inpB;
        out.c = rg_inpC;
        out.prod      = mul_result;
        out.sum_par   = res5.sum;
        out.carry_par = res5.carryOut;
        out.rpa_sum   = sumf;
        out.rpa_carry = y.carryOut;
        
        mac_st5_fifo.enq(out);
        mac_st4_fifo.deq();
    endrule : stage_5_rl

    rule stage_6_rl;

        MAC_interm inp = mac_st5_fifo.first();
        Bit#(8) rg_inpA = 'b0;
        Bit#(8) rg_inpB = 'b0;
        Bit#(32) rg_inpC = 'b0;

        rg_inpA = inp.a;
        rg_inpB = inp.b;
        rg_inpC = inp.c;
        Bit#(16) mul_result = inp.prod;
        Bit#(32) sumf      = inp.rpa_sum;

        Bit#(8) a_6='b0;
        Bit#(8) b_6='b0;
        Bit#(8) cin_6='b0;

        a_6[6:0]       = inp.sum_par[7:1];
        a_6[7]         = ~(rg_inpA[7]&rg_inpB[6]);
        b_6[0]         = 1'b0;
        b_6[7:1]       = ~(rg_inpA[6:0] & signExtend(rg_inpB[7]));
        cin_6          = inp.carry_par;
        let res6       = aa8[5].result(a_6,b_6,cin_6);
        mul_result[6]  = res6.sum[0];

        let y = fulladdition(rg_inpC[5],mul_result[5],inp.rpa_carry);
        sumf[5] = y.sum;


        MAC_interm out;
        out.a = rg_inpA;
        out.b = rg_inpB;
        out.c = rg_inpC;
        out.prod      = mul_result;
        out.sum_par   = res6.sum;
        out.carry_par = res6.carryOut;
        out.rpa_sum   = sumf;
        out.rpa_carry = y.carryOut;
        
        mac_st6_fifo.enq(out);
        mac_st5_fifo.deq();
    endrule : stage_6_rl

    rule stage_7_rl;

        MAC_interm inp = mac_st6_fifo.first();
        Bit#(8) rg_inpA = 'b0;
        Bit#(8) rg_inpB = 'b0;
        Bit#(32) rg_inpC = 'b0;

        rg_inpA = inp.a;
        rg_inpB = inp.b;
        rg_inpC = inp.c;
        Bit#(16) mul_result = inp.prod;
        Bit#(32) sumf      = inp.rpa_sum;

        Bit#(9) a_7='b0;
        Bit#(9) b_7='b0;

        a_7[6:0]       = inp.sum_par[7:1];
        a_7[7]         = rg_inpA[7]&rg_inpB[7];
        a_7[8]         = 1'b0;
        b_7[8]         = 1'b1;
        b_7[7:0]       = inp.carry_par[7:0];

        let rca9_res1     = rca9.result(a_7,b_7,1'b0);
        mul_result[15:7]  = rca9_res1.sum;

        let y = fulladdition(rg_inpC[6],mul_result[6],inp.rpa_carry);
        sumf[6] = y.sum;

        MAC_interm9 out;
        out.a = rg_inpA;
        out.b = rg_inpB;
        out.c = rg_inpC;
        out.prod      = mul_result;
        out.sum_par9   = rca9_res1.sum;
        // out.carry_par = rca9_res1.overflow;
        out.rpa_sum   = sumf;
        out.rpa_carry = y.carryOut;
        
        mac_st7_fifo.enq(out);
        mac_st6_fifo.deq();
    endrule : stage_7_rl

    rule stage_8_rl;
        MAC_interm9 inp = mac_st7_fifo.first();
        Bit#(8) rg_inpA = 'b0;
        Bit#(8) rg_inpB = 'b0;
        Bit#(32) rg_inpC = 'b0;

        rg_inpA = inp.a;
        rg_inpB = inp.b;
        rg_inpC = inp.c;
        Bit#(16) mul_result = inp.prod;
        Bit#(32) sumf      = inp.rpa_sum;

        Bit#(32) ab = signExtend(mul_result);
        Bit#(32) c   = rg_inpC;

        let d = ab[31:7];
        let e = c[31:7];

        let res_adder = rca25.result(d,e,inp.rpa_carry);

        Bool over = False;
        if((d[24]==e[24])&&(d[24]!=res_adder.sum[24])) begin
            over = True;
        end

        Bit#(32) sum_op = sumf;
        sum_op[31:7] = res_adder.sum;
        MAC_op out;
        out.result = sum_op;
        out.overflow = over;
        
        mac_op_fifo.enq(out);

        mac_st7_fifo.deq();
    endrule : stage_8_rl


    method Action start(Bit#(8) a, Bit#(8) b, Bit#(32) c);
        MAC_input x;
        x.a_inp = a;
        x.b_inp = b;
        x.c_inp = c;
        mac_inp_fifo.enq(x);
    endmethod : start

    method ActionValue#(Bit#(32)) result();
        MAC_op out = mac_op_fifo.first();
        Bit#(32) sum = out.result;
        mac_op_fifo.deq();
        return sum;
    endmethod : result

endmodule : mkMAC_int32_pipelined

endpackage : MAC_int32_pipelined
