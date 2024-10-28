import os
import random
from pathlib import Path

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge
from cocotb_coverage.coverage import *
# from model_mac32 import *

def mac32_model(a,b,c):
    return (a*b) + c

@cocotb.test()
async def test_mac_s1(dut):
    """ Test to check the working of Integer MAC """

    # collecting the testcases
    with open('A_decimal.txt', 'r') as f:
        A_values = [int(line.strip()) for line in f.readlines() if line.strip()]
    with open('B_decimal.txt', 'r') as f:
        B_values = [int(line.strip()) for line in f.readlines() if line.strip()]
    with open('C_decimal.txt', 'r') as f:
        C_values = [int(line.strip()) for line in f.readlines() if line.strip()]
    with open('MAC_decimal.txt', 'r') as f:
        expected_outputs = [int(line.strip()) for line in f.readlines() if line.strip()]
    
    dut.EN_start.value = 0

    # setting up the clk
    clock = Clock(dut.CLK, 10, units="us")
    cocotb.start_soon(clock.start(start_high=False))

    # reset
    dut.RST_N.value = 0
    for _ in range(2):
        await RisingEdge(dut.CLK)
    dut.RST_N.value = 1

    # testing the testcases
    dut.EN_start.value = 1
    for i in range(len(A_values)):
        dut.RST_N.value = 0
        for _ in range(2):
            await RisingEdge(dut.CLK)
        dut.RST_N.value = 1
        dut.start_a.value = A_values[i]
        dut.start_b.value = B_values[i]
        dut.start_c.value = C_values[i]
        expected_output = expected_outputs[i]

        dut._log.info(f'Testing case {i+1}')

        while (dut.RDY_result.value != 1):
            await RisingEdge(dut.CLK)

        result = dut.result.value.signed_integer
        dut._log.info(f'Output (signed) {result}')

        assert (expected_output) == (result), f'Counter Output Mismatch, Expected = {expected_output} DUT = {int(dut.result.value)}'
    
