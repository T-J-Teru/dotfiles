package Boolean;

=pod

=head1 SUMMARY

I like to have True and False defined for me.
This module exports two constants, True and False,
these can be used as you would expect.

=head1 START EXAMPLES

  my $var = True;
 
  if($var) {
    print "It was true.\n";
  }
  else {
    print "It was false.\n";
  }

  $SIG{INT} = sub {
    $var = False;
  }

  while ($var) {
    print "Still true.\n";
  }

=head1 END EXAMPLES

=cut

use Exporter;
use base qw/Exporter/;

our @EXPORT = qw/True False/;
use constant True  => 1;
use constant False => 0;

=pod

=head1 AUTHOR

Andrew Burgess, 2 April 2003

=cut
