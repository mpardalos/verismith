
# Table of Contents

1.  [Introduction](#org23031ec)
2.  [Finding failures in Yosys 0.8](#org081925a)
    1.  [Installing Yosys master](#orgb118ae3)
    2.  [Running Verismith](#org8276594)



<a id="org23031ec"></a>

# Introduction

The version of Verismith that is assumed to be used is Verismith 0.6.0.2, which is also available on [hackage](<https://hackage.haskell.org/package/verismith-0.6.0.2>) using:

    cabal install verismith


<a id="org081925a"></a>

# Finding failures in Yosys 0.8

Yosys 0.8 was found to fail about 30% of the time, which means that it should be quite simple to find errors in it. However, different versions of Yosys can be tested this way as well and should also result in failures, such as Yosys 0.9 or Yosys commit hashes 3333e00 or 70d0f38.

However, to find failures in Yosys 0.8, a newer version of Yosys has to be used for the equivalence check. For this we can use Yosys master. An alternative for this is to use a simulator with a testbench, which is also supported by Verismith using Icarus Verilog.


<a id="orgb118ae3"></a>

## Installing Yosys master

The first step is to install Yosys master (which will in this case be installed to `/opt/yosys/master`):

    git clone https://github.com/yosyshq/yosys && cd yosys
    sed -i 's:^PREFIX ?=.*:PREFIX ?= /opt/yosys/master:' Makefile
    make -j4
    sudo make install

Then we want to install Yosys 0.8 (which will be installed to `/opt/yosys/0.8`):

    git clean -dfx && git reset --hard HEAD
    git checkout yosys-0.8
    sed -i 's:^PREFIX ?=.*:PREFIX ?= /opt/yosys/0.8:' Makefile
    make -j4
    sudo make install


<a id="org8276594"></a>

## Running Verismith

We are then ready to run Verismith using the two Yosys versions that were installed.

Using the following config file:

    [info]
      commit = "UNKNOWN"
      version = "0.6.0.2"
    
    [probability]
      expr.binary = 5
      expr.concatenation = 3
      expr.number = 1
      expr.rangeselect = 5
      expr.signed = 5
      expr.string = 0
      expr.ternary = 5
      expr.unary = 5
      expr.unsigned = 5
      expr.variable = 5
      moditem.assign = 5
      moditem.combinational = 0
      moditem.instantiation = 1
      moditem.sequential = 1
      statement.blocking = 0
      statement.conditional = 1
      statement.forloop = 1
      statement.nonblocking = 5
    
    [property]
      determinism = 1
      module.depth = 2
      module.max = 5
      nondeterminism = 0
      output.combine = false
      sample.method = "hat"
      sample.size = 10
      size = 20
      statement.depth = 3
      default.yosys = "/opt/yosys/master"
    
    [[synthesiser]]
      bin = "/opt/yosys/0.8/bin"
      description = "yosys_0_8"
      name = "yosys"
      output = "syn_yosys_0_8.v"
    
    [[synthesiser]]
      bin = "/opt/yosys/master"
      description = "yosys_master"
      name = "yosys"
      output = "syn_yosys_master.v"
