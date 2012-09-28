#!/usr/bin/env octave -qf

# CMA-ES wrapper
# Copyright (c) 2010-2012 Shun Sakuraba
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# Usage:
# 1. Download CMA-ES implementation ("cmaes.m") from author's page ( http://www.lri.fr/~hansen/cmaes_inmatlab.html#matlab ), and put to the same directory with this file.
# 2. Write your own function evaluator and make it as an executable file.
# 3. Run this program by "./cmaes_wrap.m (your function evaluator program) (args to your function evaluator program)" on command-line.
#    Example: % octave -qf cmaes_wrap.m ./example.py
#             % octave -qf cmaes_wrap.m ./foobar important.data --magic-parameter=42
# If you rewrite your optimization function, you need to delete (function name)_save.mat before restarting calculation. This is a feature of cmaes.m.

# Synopsis of your evaluator program: (">" is from standard input and "<" is to standard output)
# > parameter
# < parameter test 3 exact 1e-4 0.0 0.1 0.3 0.4 0.5 0.2
# > evaluate 3
# > 0.034 0.237 0.601
# > -0.027 0.457 0.522
# > 0.032 0.388 0.798
# < evaluate 0.55 0.23 1.34
# > evaluate 3
# > 0.037 -0.110 0.394
# > -0.028 0.154 0.122
# > 0.091 0.097 0.423
# < evaluate 1.05 0.06 3.22
# > exit

# Your function evaluator program must read a line from the standard input, and respond to the command in a following manner:
# 1. "parameter" command. If input line is "parameter", your program must respond the following values in a single line. Note that, it is DISALLOWED to have more than 1 spaces between parameters.
# parameter <Name of function> <Dimension> <Noise> <Tolerance> <x0> <s0> <x1> <s1> ... <xn-1> <sn-1>
#    The line must start with "parameter".
#    <Name of function> is arbitrary string without space character.
#    <Dimension> is the input dimension of your function.
#    <Noise> is the noise level. Specify "exact" if the function evaluation is exact.
#    <Tolerance> decides when to stop the optimizer.
#    <x0> <s0> specify the initial value of 0-th dimension and its initial standard deviation. These values are used to set the starting point of the optimizer.
#
# 2. "evaluate" command. If input line starts with "evaluate", your program must evaluate the function values.
#    "evaluate" command accompanies a single integer <N>, which indicates the number of points simultaneously evaluated. Users can design their own parallel evaluator using this.
#    Your program must read <N> following lines, which represents <Dimension>-dimensional input coordinate of your function.
#    Your program must return "evaluate" followed by <N> values evaluated, in the order of input. If the input data is out-of-range, user may return "nan" instead of function value.
# 3. "exit" command. The evaluation program must shut down the program.


#source("cmaes.m")

# global opipe = stdin;
# global ipipe = stdout;
# 
# function line = getline
#   global opipe;
#   EAGAIN = errno("EAGAIN");
#   do
#     fclear(opipe);
#     s=fgets(opipe)
#     if(ischar(s))
#       line = s;
#       return;
#     elseif(errno() == EAGAIN)
#       sleep(0.1);
#     else
#       disp("Failed to read line from subprocess!");
#       exit(1);
#     endif
#   until(false)
# endfunction

function line = getline
  line= fgets(stdin)
endfunction

# function putline(str)
#   global ipipe;
#   EAGAIN = errno("EAGAIN");
#   EOF = errno('EOF');
#   fputs(stderr,str);
#   do
#     r=fputs(ipipe, ["<CMAES_WRAP_HS> " str "\n"])
#     if(r == EOF)
#       disp("Failed to send line to subprocess!");
#       exit(1);
#     else
#       fflush(ipipe);
#       return;
#     endif
#   until(false)
# endfunction

function putline(str)
  r=fputs(stdout, ["<CMAES_WRAP_HS> " str "\n"])
  fflush(stdout)
endfunction


putline("parameter");
params = getline();

opts.CMA.active = 1
sp = strsplit(params, " ");
kwd = sscanf(sp{:,1}, "%s");
assert(strcmp(kwd, "parameter"));
jobname = sscanf(sp{:, 2}, "%s");
N = sscanf(sp{:,3}, "%d");
noisemode = sscanf(sp{:,4}, "%s");
if(strcmp(noisemode, "noise"))
  opts.Noise.on=1;
end

opts.DispModulo = 0
opts.TolFun = sscanf(sp{:,5}, "%f");

opts.SaveFilename = [jobname "_save.mat"];
[info err msg] = stat(opts.SaveFilename);
if(err == 0)
  opts.Resume='yes';
end
opts.EvalParallel='yes';

for k = 1:N
  xinival(k) = sscanf(sp{:,4 + k * 2}, "%f");
  xinisig(k) = sscanf(sp{:,5 + k * 2}, "%f");
end

xinival
xinisig

function res = pipedeval(xs, args)
  N = size(xs, 2);
  msg = sprintf("evaluate %d", N)
  putline(msg);
  for i=1:N
    msg = sprintf("%.14e ", xs(:, i))
    putline(msg);
  end
  reply = getline()
  sp = strsplit(reply, " ");
  for i=1:N
    res(i) = sscanf(sp{:,i+1}, "%f");
  end
endfunction

xmin = cmaes('pipedeval', xinival', xinisig', opts, {})
putline("success %s\n", sprintf("%.14e ", xmin))
