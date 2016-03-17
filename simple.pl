#!/usr/bin/perl

use strict;
use warnings;
use v5.20;

use Data::Dumper;
use Storable 'dclone';

use constant Atom	=> 'atom';
use constant Var	=> 'var';
use constant Composite	=> 'composite';


my $line;
my @chars;
my $pos = 0;

my $rename_counter = 0;

my $program = [];
my $goal;

my %vars = ();


sub var_list {
	my $term = shift;
	my @vars = ();

	if ($term->{type} eq Var) {
		push @vars, $term->{name};
	}
	if ($term->{type} eq Composite) {
		for my $arg (@{$term->{args}}) {
			push @vars, var_list($arg);
		}
	}

	return @vars;
}


sub get_var_terms {
	my $term = shift;
	my @vars = ();

	if ($term->{type} eq Var) {
		push @vars, $term;
	}
	if ($term->{type} eq Composite) {
		for my $arg (@{$term->{args}}) {
			push @vars, get_var_terms($arg);
		}
	}

	return @vars;
}


sub rename_vars {
	my ($term, $renames) = @_;

	if ($term->{type} eq Var) {
		$term->{name} = $renames->{$term->{name}};
	}
	if ($term->{type} eq Composite) {
		for my $arg (@{$term->{args}}) {
			rename_vars($arg, $renames);
		}
	}
}


sub renaming_scheme {
	my $clause = shift;
	my @vars = ( var_list($clause->{head}) );

	for my $term (@{$clause->{body}}) {
		push @vars, var_list $term;
	}

	my %renames = map { $_ => $_ . "'" } @vars;

	return \%renames;
}


sub rename_vars_clause {
	my $clause = shift;
	my $renames = shift;

	rename_vars $clause->{head}, $renames;
	
	for my $term (@{$clause->{body}}) {
		rename_vars $term, $renames;
	}

	$rename_counter++;
}


sub undo_rename_vars_clause {
	my $clause = shift;
	my $renames_orig = shift;

	my $renames = { reverse %$renames_orig };

	rename_vars $clause->{head}, $renames;
	
	for my $term (@{$clause->{body}}) {
		rename_vars $term, $renames;
	}
}


sub parse_term {
	my $term = {
		name => '',
		args => []
	};

	for (; $pos < @chars; $pos++) {
		last if $chars[$pos] !~ /[a-zA-Z0-9_]/;
		$term->{name} .= $chars[$pos];
	}

	if ($chars[$pos] eq '.' or $chars[$pos] eq ',' or $chars[$pos] eq ')') {
		if ($term->{name} =~ /^[A-Z]/) {
			$term->{type} = Var;
		}
		else {
			$term->{type} = Atom;
		}

		return $term;
	}

	if ($chars[$pos] ne '(') {
		die "$0: parse error: expected either '.', ',', '(' or ')'";
	}

	$pos++; # skip the '('

	for (; $pos < @chars; $pos++) {
		push @{$term->{args}}, parse_term();
		next if $chars[$pos] eq ',';
		last;
	}

	if ($pos >= @chars or $chars[$pos] ne ')') {
		die "$0: missing final ')'";
	}

	$pos++; # skip the ')'

	$term->{type} = Composite;
	return $term;
}


sub parse_rule {
	my $rule = {
		body => [],
	};

	$rule->{head} = parse_term;

	if ($chars[$pos] eq '.') {
		return $rule;
	}

	if ($chars[$pos] eq ':' && $chars[$pos + 1] eq '-') {
		$pos += 2;
	}

	for (; $pos < @chars; $pos++) {
		push @{$rule->{body}}, parse_term;
		next if $chars[$pos] eq ',';
		last;
	}

	if ($chars[$pos] ne '.') {
		die "$0: expected dot ('.')";
	}

	return $rule;
}


sub term_to_str {
	my $term = shift;
	my $str = '';

	$str .= $term->{name};

	if ($term->{type} eq Composite) {
		$str .= '(';
		$str .= join ',', map { term_to_str($_); } @{$term->{args}};
		$str .= ')';
	}

	return $str;
}


sub clause_to_str {
	my $clause = shift;

	my $str = term_to_str($clause->{head});

	if (@{$clause->{body}} > 0) {
		$str .= ' :- ';
		$str .= join ',', map { term_to_str($_); } @{$clause->{body}};
	}

	return $str;
}


sub find_mgu {
	my ($t, $u) = @_;

	if ($t->{type} eq Atom && $u->{type} eq Atom && $t->{name} eq $u->{name}) {
		return {};
	}

	if ($t->{type} eq Var && $u->{type} eq Var) {
		return {$t->{name} => $u };
	}

	if ($t->{type} eq Var && $u->{type} ne Var) {
		return { $t->{name} => $u };
	}

	if ($t->{type} ne Var && $u->{type} eq Var) {
		return { $u->{name} => $t };
	}

	my $tc = dclone $t;
	my $uc = dclone $u;

	if ($tc->{type} eq Composite && $uc->{type} eq Composite) {
		my $subst = {};

		if (not $tc->{name} eq $uc->{name}) {
			return;
		}

		if (scalar @{$tc->{args}} != scalar @{$uc->{args}}) {
			return;
		}

		my $substs = {};
		for (my $i = 0; $i < @{$tc->{args}}; $i++) {
			$subst = find_mgu($tc->{args}->[$i], $uc->{args}->[$i]);
			return undef if not defined $subst;

			do_subst($subst, $tc->{args});
			do_subst($subst, $uc->{args});

			for my $key (keys %$subst) {
				$substs->{$key} = $subst->{$key};
			}
		}

		return $substs;
	}

	return undef;
}


sub do_subst {
	my ($subst, $goals) = @_;

	for my $goal (@$goals) {
		if ($goal->{type} eq Var) {
			my $replace = $subst->{$goal->{name}};
			next if not defined $replace;

			%$goal = %$replace;
		}

		if ($goal->{type} eq Composite) {
			do_subst($subst, $goal->{args});
		}
	}
}


sub reach {
	my $goals = shift;
	my $local_program = dclone $program;

	if (scalar @$goals == 0) {
		return 1;
	}

	my $done = 0;
	my $goal = shift @$goals;

	for my $clause (@$local_program) {
		my $renames = renaming_scheme $clause;
		rename_vars_clause $clause, $renames;;
		my $subst = find_mgu $goal, $clause->{head};

		if (defined $subst) {
			say "Using ", clause_to_str $clause;
			for my $key (keys %$subst) {
				say "$key = ", term_to_str $subst->{$key};
			}

			my $new_goals = dclone [ @{$clause->{body}}, @$goals ];
			
			do_subst $subst, $new_goals;
			do_subst $subst, [values %vars];

			undo_rename_vars_clause $clause, $renames;

			$done = reach($new_goals);
			if (!$done) {
				say "\tfalse.";
			}
			else {
				say "Goal reached!";
				for my $name (keys %vars) {
					say "$name = ", term_to_str $vars{$name};
				}
			}
		}
		else {
			undo_rename_vars_clause $clause, $renames;
		}

		#last if $done;
	}

	return $done;
}

say "Program:";
while ($line = <>) {
	next if $line =~ /^$/;

	chomp $line;
	@chars = split //, $line;
	$pos = 0;

	my $clause = parse_rule;
	push @$program, $clause;

	say "\t", (clause_to_str $clause), '.';
}
say "";

while (1) {
	print ":- ";
	$line = <>;

	last if not defined $line;

	@chars = split //, $line;
	$pos = 0;

	$goal = parse_term;
	%vars = map { ($_->{name} => dclone $_) } get_var_terms $goal;

	say +(reach [$goal]) ? 'true.' : 'false.';
}
