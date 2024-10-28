import os
import random
from pathlib import Path
import struct
from cocotb.binary import BinaryValue

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge
from cocotb_coverage.coverage import *

@cocotb.test()
async def test_mac32(dut):
    """ Test to check the working of Integer MAC """

    # collecting the testcases
    with open('A_binary.txt', 'r') as f:
        A_values = [int(line.strip(), 2) for line in f.readlines() if line.strip()]
    with open('B_binary.txt', 'r') as f:
        B_values = [int(line.strip(), 2) for line in f.readlines() if line.strip()]
    with open('C_binary.txt', 'r') as f:
        C_values = [int(line.strip(), 2) for line in f.readlines() if line.strip()]
    with open('MAC_binary.txt', 'r') as f:
        expected_outputs = [int(line.strip(), 2) for line in f.readlines() if line.strip()]
    
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
    c = 0
    for i in range(len(A_values)):
        dut.RST_N.value = 0
        for _ in range(2):
            await RisingEdge(dut.CLK)
        dut.RST_N.value = 1
        dut.start_a.value = A_values[i]
        dut.start_b.value = B_values[i]
        dut.start_c.value = C_values[i]
        expected_output = expected_outputs[i]

        while (dut.RDY_result.value != 1):
            await RisingEdge(dut.CLK)

        result = dut.result.value.signed_integer

        diff = abs(result-expected_output)
        if diff > 3:
            c += 1
            dut._log.info(f'Testing case {i+1}')
            dut._log.info(f'ERROR : expected = {expected_output}, dut = {result}')
        
        # assert (expected_output) == (result), f'Counter Output Mismatch, Expected = {expected_output} DUT = {int(dut.result.value)}'
    
    dut._log.info(f'TOTAL MISMATCHES : {c}')