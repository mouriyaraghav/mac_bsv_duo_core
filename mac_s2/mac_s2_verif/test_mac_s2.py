import os
import random
from pathlib import Path
import cocotb
from cocotb_coverage.crv import *
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge
from cocotb_coverage.coverage import *
import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'
import tensorflow as tf

num_bins = 2*10

min_c = -2**31
max_c = 2**31 - 1
bin_width = (max_c - min_c + 1) // num_bins

# min_ab = -16256
# max_ab = 16384
# bin_width_ab = (max_ab - min_ab + 1) // num_bins

counter_coverage = coverage_section(
    CoverPoint("top.a", xf=lambda a, b, c: a, bins=list(range(-128,128))),
    CoverPoint("top.b", xf=lambda a, b, c: b, bins=list(range(-128,128))),
    CoverPoint("top.c", xf=lambda a, b, c: f"bin_{(c - min_c) // bin_width}", bins=[f"bin_{i}" for i in range(num_bins)]),
    CoverCross("top.cross_ab", items=["top.a", "top.b"]),
    # CoverCross("top.cross_abc", items=["top.cross_ab", "top.c"])
)
@counter_coverage
def mac_s2(A_bits: int, B_bits: int, C_bits: int) -> tf.Tensor:
    A = tf.raw_ops.Bitcast(input=tf.constant(A_bits, dtype=tf.uint16), type=tf.bfloat16)
    B = tf.raw_ops.Bitcast(input=tf.constant(B_bits, dtype=tf.uint16), type=tf.bfloat16)
    C = tf.raw_ops.Bitcast(input=tf.constant(C_bits, dtype=tf.uint32), type=tf.float32)
    result = tf.cast(A * B, tf.float32) + C

    result_bits = tf.raw_ops.Bitcast(input=result, type=tf.uint32)
    return float(result_bits)

@cocotb.test()
async def test_mac32(dut):
    """ Test to check the working of Integer MAC """
    
    dut.EN_start.value = 0

    # setting up the clk
    clock = Clock(dut.CLK, 10, units="us")
    cocotb.start_soon(clock.start(start_high=False))

    # reset
    dut.RST_N.value = 0
    for _ in range(2):
        await RisingEdge(dut.CLK)
    dut.RST_N.value = 1

    # drive inputs
    dut.EN_start.value = 1
    seed = 42
    for a in range(-128,128):
        for b in range(-128,128):
            seed+=1
            random.seed(seed)
            dut.RST_N.value = 0
            for _ in range(2):
                await RisingEdge(dut.CLK)
            dut.RST_N.value = 1
            
            c = random.randint(min_c,max_c)
            dut.start_a.value = a
            dut.start_b.value = b
            dut.start_c.value = c
            expected_output = mac_s2(a, b, c)

            # dut._log.info(f'Testing case')
            while (dut.RDY_result.value != 1):
                await RisingEdge(dut.CLK)
            
            result = dut.result.value.signed_integer
            # dut._log.info(f'output (signed) {result}')

            assert (expected_output) == (result), f'Counter Output Mismatch, Expected = {int(expected_output)} DUT = {int(dut.result.value)}'
        dut._log.info(f'Run {a+128}')

    for i in range(2**8):
        random.seed(seed+i*2)
        dut.RST_N.value = 0
        for _ in range(2):
            await RisingEdge(dut.CLK)
        dut.RST_N.value = 1
        
        a = random.randint(-128,127)
        b = random.randint(-128,127)
        c = random.randint(min_c,max_c)
        dut.start_a.value = a
        dut.start_b.value = b
        dut.start_c.value = c
        expected_output = mac_s2(a, b, c)

        # dut._log.info(f'Testing case')
        while (dut.RDY_result.value != 1):
            await RisingEdge(dut.CLK)
        
        result = dut.result.value.signed_integer
        # dut._log.info(f'output (signed) {result}')

        assert (expected_output) == (result), f'Counter Output Mismatch, Expected = {int(expected_output)} DUT = {int(dut.result.value)}'
        dut._log.info(f'Run 2 {i}')

    coverage_db.export_to_yaml(filename="coverage_ab_c_new.yml")
