#!/usr/local/bin/perl
use strict;
use warnings;
use Proc::ProcessTable;
 
my $table = Proc::ProcessTable->new;
 
for my $process (@{$table->table}) {
    # skip root processes
    next if $process->uid == 0 or $process->gid == 0;
 
    # skip anything other than Passenger application processes
    next unless $process->fname eq 'java' #and $process->cmndline =~ /\bRails\b/;
 
    # skip any using less than 1 GiB
    next if $process->rss < 1_073_741_824;
 
    # document the slaughter
    (my $cmd = $process->cmndline) =~ s/\s+\z//;
    print "Processo guloso: pid=", $process->pid, " uid=", $process->uid, " rss=", $process->rss, " fname=", $process->fname, " cmndline=", $cmd, "\n";
 
    # try first to terminate process politely
    #kill 15, $process->pid;
}
