import os
import random
from pathlib import Path
import cocotb
from cocotb_coverage.crv import *
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge
from cocotb_coverage.coverage import *

# splitting the bins with a limited precision
num_bins = 2*10

min_c = -2**31
max_c = 2**31 - 1
bin_width = (max_c - min_c + 1) // num_bins

# ab possibilities
ab = set()
for a in range(-128, 128):
    for b in range(-128, 128):
        ab.add(a * b)
ab_bins = sorted(ab)

bins_ab = [f"bin_{p}" for p in ab_bins]

counter_coverage = coverage_section(
    CoverPoint("top.a", xf=lambda a, b, c: a, bins=list(range(-128,128))),
    CoverPoint("top.b", xf=lambda a, b, c: b, bins=list(range(-128,128))),
    CoverPoint("top.c", xf=lambda a, b, c: f"bin_{(c - min_c) // bin_width}", bins=[f"bin_{i}" for i in range(num_bins)]),
    CoverCross("top.cross_ab", items=["top.a", "top.b"]),
    CoverPoint("top.ab", xf=lambda a, b, c: f"bin_{a * b}", bins=bins_ab),
    CoverCross("top.cross_abc", items=["top.ab", "top.c"])
)
@counter_coverage
def mac_s1(a,b,c):
    return (a*b) + c

@cocotb.test()
async def test_mac_s1(dut):
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
            seed += 1
            random.seed(seed)
            dut.RST_N.value = 0
            for _ in range(2):
                await RisingEdge(dut.CLK)
            dut.RST_N.value = 1
            
            c = random.randint(min_c,max_c)
            dut.start_a.value = a
            dut.start_b.value = b
            dut.start_c.value = c
            expected_output = mac_s1(a, b, c)

            while (dut.RDY_result.value != 1):
                await RisingEdge(dut.CLK)
            
            result = dut.result.value.signed_integer
            # dut._log.info(f'output (signed) {result}')
            assert (expected_output) == (result), f'MAC Output Mismatch, Expected = {int(expected_output)} DUT = {int(dut.result.value)}'
        
        dut._log.info(f'Run {a+128}')

    # taking care of the extreme cases explicitly
    corner_cases = [
        (0, 0, 0),
        (-1, 1, -1),
        (-128, 127, min_c),
        (-128, 127, max_c),
        (127, -128, max_c),
        (127, -128, min_c), 
        (-128, -128, min_c),
        (-128, -128, max_c),
        (127, 127, max_c),
        (127, 127, min_c)
    ]

    for a, b, c in corner_cases:
        dut.RST_N.value = 0
        for _ in range(2):
            await RisingEdge(dut.CLK)
        dut.RST_N.value = 1

        dut.start_a.value = a
        dut.start_b.value = b
        dut.start_c.value = c
        expected_output = mac_s1(a, b, c)

        dut._log.info(f"Corner Case Tested: A={a}, B={b}, C={c}")
        while dut.RDY_result.value != 1:
            await RisingEdge(dut.CLK)
        
        result = dut.result.value.signed_integer
        assert (expected_output) == (result), f'MAC Output Mismatch, Expected = {int(expected_output)} DUT = {int(dut.result.value)}'

    # walking 1s and walking 0s
    walk = []
    for i in range(8):
        walk.append((1 << i, 0, 0))
        walk.append((~(1 << i) & 0xFF, 0, 0))

    for i in range(8):
        walk.append((0, 1 << i, 0))
        walk.append((0, ~(1 << i) & 0xFF, 0))

    for i in range(32):
        walk.append((0, 0, 1 << i))
        walk.append((0, 0, ~(1 << i) & 0xFFFFFFFF))

    for a, b, c in walk:
        dut.RST_N.value = 0
        for _ in range(2):
            await RisingEdge(dut.CLK)
        dut.RST_N.value = 1

        if a < 128: pass
        else: a = a - 256
        if b < 128: pass
        else: b = b - 256
        if c <= max_c: pass
        else: c = c - (1 << 32)

        dut.start_a.value = a
        dut.start_b.value = b
        dut.start_c.value = c
        expected_output = mac_s1(a,b,c)

        dut._log.info(f"Walking 1s/0s Case Tested: A={a}, B={b}, C={c}")
        while dut.RDY_result.value != 1:
            await RisingEdge(dut.CLK)
        
        result = dut.result.value.signed_integer
        assert (expected_output) == (result), f'MAC Output Mismatch, Expected = {int(expected_output)} DUT = {int(dut.result.value)}'

    coverage_db.export_to_yaml(filename="coverage_ab_c.yml")
