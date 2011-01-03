require 'socket'

task :default => [:make_erl]

task :clean do
  print "Cleaning..."
  sh "rebar clean"
  print " done\n"
end

task :make_erl do
  print "Compiling Erlang sources...\n"
  sh "rebar get-deps && rebar compile"
  sh "erl -pa ebin -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'"
  print "Compiling C sources...\n"
  sh "cd deps/fd_server && make"
  print " done\n"
end

task :run, [:node, :config, :withsasl] do |t, args|
  args.with_defaults(:node => "dns",
                     :withsasl => "true",
                     :config => Socket.gethostname)
  sasl = args.withsasl == "true" ? "-boot start_sasl" : ""
  configarg = File.exist?("#{args.config}.config") ? "-config #{args.config}" : ""
  cmdline = "erl +W w #{configarg} -name #{args.node} #{sasl} -setcookie dns -pa ebin -pa deps/fd_server/ebin -s crypto -s inets +Bc +K true -smp enable"
  puts cmdline
  sh cmdline
end

task :build_plt do
  sh 'dialyzer ebin --build_plt --apps erts kernel stdlib inets crypto eunit'
  sh 'mv ~/.dialyzer_plt ~/.gen_dns_dialyzer_plt'
end

task :analyze do
  sh 'dialyzer --plt ~/.gen_dns_dialyzer_plt -Wunmatched_returns -Werror_handling -Wrace_conditions -Wbehaviours ebin | grep --color -e "^[^:]*:\|^[^:]*$"'
end

task :updatedeps do
  print "Updating Erlang dependencies..."
  sh "rebar delete-deps && rebar get-deps"
  print " done\n"
end

task :doc do
  sh "mkdir -p doc/html"
  sh "cp src/overview.edoc doc/html"
  sh 'erl -noshell -eval "edoc:files(filelib:wildcard(\"src/*.erl\"), [{dir, \"doc/html\"}, {includes, [\"include\"]}, {source_path, [\"include\", \"src\"]}])" -s init stop'
end

task :xref do
  sh "rebar xref"
end