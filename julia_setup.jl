# using Base.Terminals: UnixTerminal
# using Base.REPL: BasicREPL, run_repl

# type PipeTerminal <: UnixTerminal
#     in_stream::Base.Pipe
#     out_stream::Base.Pipe
#     err_stream::Base.Pipe
# end

# run_repl(BasicREPL(PipeTerminal(STDIN,STDOUT,STDERR)))

using Base: stdin, stdout, stderr
using REPL.Terminals: TTYTerminal
using REPL: BasicREPL, run_repl

run_repl(BasicREPL(TTYTerminal("emacs",stdin,stdout,stderr)))

