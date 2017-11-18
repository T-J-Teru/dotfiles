#!/usr/bin/perl

use warnings;
use strict;

#========================================================================#

=pod

=head1 NAME

install.pl - install dotfiles.

=head1 OPTIONS

B<install.pl> [-h|--help] [--install-dir=<DEST>] [--verbose]

=head1 SYNOPSIS

A full description for install has not yet been written.

=cut

#========================================================================#

use FindBin;
use lib "$FindBin::Bin/../lib";
use GiveHelp qw/usage/;         # Allow -h or --help command line options.
use File::Find;
use Getopt::Long;
use Cwd qw/abs_path/;
use Carp;
use Carp::Assert;
use Boolean;

my $install_dir = undef;
my $src_dir = undef;
my $verbose = False;
GetOptions ("install-dir=s" => \$install_dir,
            "source-dir=s" => \$src_dir,
            "verbose" => \$verbose);

if (not (defined ($install_dir)))
{
  # Not overridden the install directory, so install into home.
  die "Not defaulting to home just yet";
}

if (not (defined ($src_dir)))
{
  $src_dir = "$FindBin::Bin/../home";
}

(-d $install_dir) or
  die "Installation directory '$install_dir' not found";

(-d $src_dir) or
  die "Source directory '$src_dir' not found";

$src_dir = abs_path ($src_dir);
my $src_root = abs_path ($src_dir."/..");
$install_dir = abs_path ($install_dir);

#========================================================================#

find (\&install_generic, ($src_dir));

exit (0);

#========================================================================#

=pod

=head1 METHODS

The following methods are defined in this script.

=over 4

=cut

#========================================================================#

=pod

=item B<ignore_file>

Return true if a file should not be installed.  First parameter is the full
path to the file (including the filename) and the second parameter is just
the filename.

=cut

sub ignore_file {
  my $full_path = shift;
  my $filename = shift;

  assert (-f $full_path);
  return (($filename =~ m/^\./) or ($filename =~ m/~$/));
}

#========================================================================#

=pod

=item B<verbose>

Currently undocumented.

=cut

sub verbose {
  return unless ($verbose);

  print foreach (@_);
}

#========================================================================#

=pod

=item B<filter_dest_path>

Take a path and convert instances of "DOT_" into "." when they occur either
after a "/" or at the very start of the path. Returns the updted path.

=cut

sub filter_dest_path {
  my $dest_path = shift;
  $dest_path =~ s,/DOT_,/\.,g;
  $dest_path =~ s,^DOT_,\.,;
  return $dest_path;
}

#========================================================================#

=pod

=item B<src_to_dest>

Take a single parameter, a source path, return a destination path.

=cut

sub src_to_dest {
  my $src_path = shift;

  my $rel_path = $src_path;
  $rel_path =~ s,^$src_dir,,;

  my $dest_path = ${install_dir}.${rel_path};
  $dest_path = filter_dest_path ($dest_path);

  return ($dest_path);
}

#========================================================================#

=pod

=item B<install_generic>

Call back from the File::Find::find function.  No parameters passed in,
value are passed in global variables.  See the perldoc for File::Find for
more details.

=cut

sub install_generic {
  my $src_path = $File::Find::name;
  my $filename = $_;
  my $dest_path = src_to_dest ($src_path);

  # Based on the type of the source path, install the source object.
  if (-l $src_path)
  {
    install_symlink ($src_path, $dest_path);
  }
  elsif (-d $src_path)
  {
    install_directory ($src_path, $dest_path);
  }
  elsif (-f $src_path)
  {
    if (not ignore_file ($src_path, $filename))
    {
      install_file ($src_path, $dest_path);
    }
  }
  else
  {
    die "Source file '$src_path' is of an unknown type\n";
  }
}

#========================================================================#

=pod

=item B<install_symlink>

Install a symlink source object.  We should make the following checks,

If the target of the symlink is relative, and within the source tree, then
recreate it in the destination.

If the target of the symlink is relative, and is one of the source helper
scripts then run the script to create the installed file.

Otherwise?  An absolute symlink?  Not sure what to do with this, error for
now.

=cut

sub install_symlink {
  my $src_path = shift;
  my $dest_path = shift;

  verbose ("Install '$src_path'\n");
  verbose ("     in '$dest_path'\n");

  my $src_link = readlink ($src_path) or
    die "Failed to read symlink '$src_path': $!";

  verbose ("  is symlink to '$src_link'\n");
  if (substr ($src_link, 0, 1) eq "/")
  {
    # Absolute symlink.
    die "Symlink in install tree '$src_path' to '$src_link' is absolute";
  }
  else
  {
    # Relative symlink
    my $abs_src_link = abs_path ($File::Find::dir."/".$src_link);
    if ($abs_src_link =~ m,^$src_dir,)
    {
      my $dest_link = filter_dest_path ($src_link);

      if (-l $dest_path)
      {
        my $curr_dest_link = readlink ($dest_path);
	if ($curr_dest_link ne $dest_link)
	{
          die "Destination '$dest_path' already exists, ".
              "but is a symlink to '$curr_dest_link' not '$dest_link'";
	}
	else
	{
          verbose ("  symlink already exists.\n");
	}
      }
      elsif (-e $dest_path)
      {
        die "Destination '$dest_path' already exists, but is not a symlink";
      }
      else
      {
        verbose ("  symlink '$dest_link'\n");
        verbose ("     ---> '$dest_path'\n");
        symlink ($dest_link, $dest_path) or
          die "Failed to create '$dest_path' to '$dest_link': $!";
      }
    }
    elsif ($abs_src_link =~ m,^$src_root,)
    {
      verbose ("  is a symlink to a helper script.\n");

      if (-x $src_path)
      {
        warn "TODO: Run helper scripts to fill in install tree";
      }
      else
      {
        die "Link '$src_path' to '$src_link' is not executable";
      }
    }
    else
    {
      # This is a symlink outside of source.
      die "Relative symlink outside of souce '$src_path'";
    }
  }
}

#========================================================================#

=pod

=item B<install_file>

Takes two paths, the path to a souce file to install, and a path to a
destination location.

If the destination does not exist, then create a symlink from the
destination to the source.

If the destination already exists then either it's already a symlink to the
source, in which case we're done, otherwise, we give an error and exit.

=cut

sub install_file {
  my $src_path = shift;
  my $dest_path = shift;

  verbose ("Linking '$dest_path'\n");
  verbose ("     to '$src_path'\n");

  if (-e $dest_path)
  {
    if (-f $dest_path)
    {
      if (-l $dest_path)
      {
        my $link = readlink ($dest_path);
        if ($link ne $src_path)
        {
          die "'$dest_path' is a file, and a link already, but to '$link' ".
            "not '$src_path'\n";
        }
        else
        {
          verbose ("  symlink already exists.\n");
        }
      }
      else
      {
        die "File '$dest_path' is a file, not a link\n";
      }
    }
    else
    {
      die "Destination '$dest_path' exists, but is not a file";
    }
  }
  else
  {
    # Destination does not exist.
    verbose ("  symlink '$src_path'\n");
    verbose ("     ---> '$dest_path'\n");
    symlink $src_path, $dest_path or
      die "Failed to create link '$dest_path' to '$src_path': $!";
  }
}

#========================================================================#

=pod

=item B<install_directory>

Take a source path and a destination path.  If the destination exists and
is a directory then we're done.  If the destination does not exist, then we
create a directory at the destination path.  If the destination exists, but
is not a directory then we error and exit.

=cut

sub install_directory {
  my $src_path = shift;
  my $dest_path = shift;

  verbose ("Create directory '$dest_path'\n");

  if (-e $dest_path)
  {
    if (-d $dest_path)
    {
      if (-l $dest_path)
      {
        die "Destination '$dest_path' exists, but is a symlink";
      }
      else
      {
        verbose ("  already exists.\n");
      }
    }
    else
    {
      die "Destination '$dest_path' exists, but is not a directory";
    }
  }
  else
  {
    verbose ("  mkdir '$dest_path'\n");
    mkdir $dest_path or
      die "Failed to create '$dest_path': $!";
  }
}

#========================================================================#

=pod

=back 4

=head1 AUTHOR

Andrew Burgess, 14 Aug 2014

=cut
