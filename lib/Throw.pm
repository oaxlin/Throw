package Throw;

=head1 NAME

Throw - Simple exceptions that do the right things in multiple contexts

=cut

use strict;
use warnings;
use overload '""' => \&_str, fallback => 1;

our ($base_level, $level, $max_args, $max_arg_len, @EXPORT, $trace, $pretty, $js, $jp, %skip) = (0, 0, 5, 20, 'throw');

sub import {
    no strict 'refs';  my ($me, $you, $f, $l) = (shift, caller); @_ = @{"$me\::EXPORT"} if !@_;
    defined &{"$me\::$_"} ? *{"$you\::$_"} = \&{"$me\::$_"} : die "Cannot export $_ from $me to $you at $f line $l.\n" for @_;
}

sub throw   { local $base_level = 1; die __PACKAGE__->new(@_) }
sub croak   { local ($base_level, $trace) = (2, 1);  die __PACKAGE__->new(@_) } # die from perspective of caller
sub carp    { local ($base_level, $trace) = (2, 1); warn __PACKAGE__->new(@_) } # warn from caller
sub confess { local ($base_level, $trace) = (1, 2);  die __PACKAGE__->new(@_) } # die with full trace
sub cluck   { local ($base_level, $trace) = (1, 2); warn __PACKAGE__->new(@_) } # warn with trace

sub new {
    my ($class, $e, $i, $l) = @_;
    if (! ref $e) {
        my %e = %{$i || {}};
        $e{'_error'} = $e{'error'} if exists $e{'error'};
        $e{'error'} = $e;
        $e = \%e;
    }
    $e->{'trace'} = caller_trace($trace || $e->{'trace'}, $l) if $trace || $e->{'trace'};
    return bless $e, $class;
}

sub _str {
    my $self = shift;
    my $err  = $self->{'error'} || "Something happened";
    my $p    = defined($pretty) ? $pretty : $self->{'_pretty'};
    local @$self{'error','_pretty'};  delete @$self{'error','_pretty'};
    return "$err\n" if !scalar keys %$self;
    require JSON;
    my $j = $p ? $jp ||= JSON->new->allow_unknown->allow_blessed->utf8->convert_blessed->canonical->pretty
               : $js ||= JSON->new->allow_unknown->allow_blessed->utf8->convert_blessed->canonical;
    return "$err: ".$j->encode({%$self}).($p ? '' : "\n");
}

sub caller_trace {
    my ($verbose, $lev) = @_;
    return $verbose if $verbose !~ /^[123]$/;
    my $i = ($lev || $level) + $base_level + 1;
    return sprintf "Called from %s at %s line %s", (caller $i+1)[3]||'main', map{s|^(?:.+/)?lib/||;$_} (caller $i)[1,2] if $verbose && $verbose eq '1';
    my ($m1, $m2, $m3, $lpkg, $nv, @trace) = (0, 0, 0, __PACKAGE__, eval {require 5.014} ? $verbose ne '3' : 1);
    while (1) {
        my ($pkg, $file, $line, $sub, $args) = $nv ? ((caller $i++)[0..3], [])
            : do { package DB; local $DB::args[0] = \$nv; ((caller $i++)[0..3], ($DB::args[0]||'') ne \$nv ? [@DB::args] : []) };
        last if ! $sub;
        $args = [] if $lpkg eq __PACKAGE__;
        $lpkg = $pkg;
        $sub =~ s/.*://; $file =~ s|^(?:.+/)?lib/||;
        next if ($file eq __FILE__) || $skip{$file} || $skip{$pkg} || $skip{$sub};
        splice @$args, $max_args, -1, '...' if @$args > $max_args;
        $args = !@$args ? '' : ' ('.join(', ',map{$_=!defined($_)?'undef':ref($_)||!/\D/?$_:do{(my$c=$_)=~s|([\'/])|\\$1|g;"'$c'"}; substr($_,0,$max_arg_len)} @$args).')';
        $m1 = length $sub  if length($sub)  > $m1;
        $m2 = length $file if length($file) > $m2;
        $m3 = length $line if length($line) > $m3;
        push @trace, [$sub, $file, $line, $args];
    }
    return join "\n", map {sprintf "%-${m1}s at %-${m2}s line %${m3}s%s", @$_} @trace;
}

sub TO_JSON { return {%{$_[0]}} }

sub classify {
    my ($err, $ref) = @_;
    $ref = {$ref => 1} if ! ref $ref;
    my $type = !ref($err) ? 'undef.flat' : defined($err->{'type'}) ? $err->{'type'} : 'undef.none';
    my @keys = grep {$_ ne 'default'} keys %$ref;
    @keys = sort {length($b) <=> length($a) || $a cmp $b} @keys if @keys > 1;
    foreach my $key (@keys) {
        next if $type !~ /^\Q$key\E\b(?:$|\.)/;
        my $val = $ref->{$key};
        return ref($val) ? $val->($err, $key) : $val;
    }
    return if ! exists $ref->{'default'};
    my $val = $ref->{'default'};
    return ref($val) ? $val->($err, 'default') : $val;
}

1;

__END__

=head1 SYNOPSIS

    use Throw qw(throw);

    throw "Hey";

    # "$@" eq "Hey\n";
    # JSON->new->allow_blessed(1)->convert_blessed(1)->encode($e) eq '{"error":"Hey"}'


    throw "Hey", {info => "This is why"};

    # "$@" eq "Hey: {\"info\":\"This is why\"}\n"
    # json->encode eq '{"info":"This is why","error":"Hey"}'


    throw "Hey", {trace => 1}; # simple trace

    throw "Hey", {trace => 2}; # full trace without args

    throw "Hey", {trace => 3}; # full trace with args


    use Throw qw(croak confess carp);

    croak "Hey";  # same as throw with {trace => 1}, 1

    confess "Hey";  # same as throw with {trace => 2}

    carp "Hey";  # warns from perspective of caller

    warn Throw->new("Hey");  # useful for some cases


    use Throw qw(throw classify);
    if (classify my $err = $@, "io") {
        throw "Got a disk error", {msg => $err};
    }

=head1 DESCRIPTION

Throw allows for light weight exceptions that can hold more
information than just the error message.  These exceptions do the
right thing when thrown on the commandline, or when consumed by
javascript based APIs.

Internally the die is stored as a hash with the error message set
as the hash key named "error" (if a hash key named error is also
passed in the hash, it will be renamed to _error).

=head1 METHODS

=over 4

=item throw

Takes an error message, an error message and extra arguments, or a
hashref.

If a hashref is passed, it should contain a key named error
representing the error.  If not, a string "Something happened" will be
used instead.  If an arguments hashref is passed, the error message
will be added to it.  If just an error message is passed, a hashref
will be created with the error as the single key.

In all cases, throw returns an hashref based object blessed into the
Throw class.

If a key of "trace" is passed, its value will be passed to the
caller_trace subroutine and the result will be stored as the value of
trace.

An optional 3rd parameter can be passed which will be used as the "level"
for any stack traces performed.

=item croak

Gives a trace from the perspective of the caller.
Similar to throw - but with trace => 1 instead.  (passing a single hashref is not allowed)
Single level stack trace.

=item confess

Similar to throw - but with trace => 2 instead.  (passing a single hashref is not allowed)
Full stack trace.

=item carp

Gives a trace from the perspective of the caller.
Similar to throw but only warns with trace => 1.

=item cluck

Similar to throw but only warns with trace => 2.

=item caller_trace

Returns stack traces.  Takes parameters in a few different ways.

     caller_trace();    # {level => 0, verbose => 2}
     caller_trace(1);   # {level => 0, verbose => 1}
     caller_trace(2);   # {level => 0, verbose => 2}
     caller_trace(3);   # {level => 0, verbose => 3}

     caller_trace(undef, 3);  # {level => 3, verbose => 1}
     caller_trace(1, 4);      # {level => 4, verbose => 1}
     caller_trace(2, 2);      # {level => 2, verbose => 2}

     caller_trace({level => 1});                # {level => 1, verbose => 2}
     caller_trace({level => 1, verbose => 3});  # {level => 1, verbose => 3}

The "level" argument represents how many stack frames to skip
backwards.

The "verbose" argument can be one of 1, 2, or 3.  Default 2.  At level
1 you get a single line of trace.  With level 2 you get the full stack
trace.  With level 3 you get the full stack trace with function
arguments.

The "max_args" argument shows how many parameters to each level will
be represented.  If there are more an "..." will be shown.  Default is
5.

The "max_arg_len" argument shows where parameters will be truncated.
Default is 20.

The "skip" argument can be a hashref with keys of packages, files, or
subs that should be excluded from the trace.

=item classify

Allows for cleanly and safely classifying the types of errors received
assuming you use {type => 'error_type'} for specifying your error
hierarchy.  Classify takes an error (such as from $@), and a hashref
used to classify the error.  Each of the keys of the hashref will be
checked against the type of the error.  The classification keys are
checked based on hierarchy - so a key of "foo" will match an error
type of "foo" as well as "foo.bar", "foo.baz", and "foo.bar.baz".  A
key of "foo.bar" would match "foo.bar" and "foo.bar.baz" but not
"foo".

You may also pass a key named "default" to handle any cases not
matched by other keys.

Some errors passed to classify may not have been given a type
property, and some may not even have been blessed or come from the
Throw system.  Any unblessed errors will receive use a type of
"undef.flat" and any other errors that do not have a type attribute
will use "undef.none" for the type.

    use Throw qw(throw classify);
    use Try::Tiny qw(try catch);

    try {
        throw "No-no", {type => 'foo.bar'};
    } catch {
        classify $_, {
            foo => sub { print "I got foo\n" },
            'foo.bar' => sub { print "I got foo.bar\n" },
            default   => sub { throw "Don't know what I got", {msg => $_[0]} }
        };
    }


    # also
    if (classify $@, "foo") {
        print "I got a foo\n";
    }

=back

=head1 GLOBALS

There are also a few package globals that can make tracking down culprits easier.

=over 4

=item $trace

Turn on traces globally - can be any of the normal values passed to trace

=item $level

Set the level at which to trace.

=item $pretty

Allow all json error stringification to use pretty.  You can also set _pretty => 1 in
individual errors, but sometimes you won't have access to the error object before
it stringifies.

=back

=cut
