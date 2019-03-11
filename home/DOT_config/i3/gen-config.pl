#!/usr/bin/perl

use warnings;
use strict;
no indirect;
no autovivification;

#========================================================================#

=pod

=head1 NAME

gen-config - a quick summary of what gen-config does.

=head1 OPTIONS

B<gen-config> [-h|--help]

=head1 SYNOPSIS

A full description for gen-config has not yet been written.

=cut

#========================================================================#

use lib "$ENV{HOME}/lib";
use GiveHelp qw/usage/;         # Allow -h or --help command line options.
use Sys::Hostname;
use File::Temp qw /tempfile/;
use Cwd qw/abs_path/;
use File::Glob qw/bsd_glob/;
use File::Copy;
use File::Compare;
use POSIX qw(strftime);

#========================================================================#

my $I3_CONFIG_DIRECTORY = "~/.config/i3/";

#========================================================================#

exit (main ());

#========================================================================#

=pod

=head1 METHODS

The following methods are defined in this script.

=over 4

=cut

#========================================================================#

=pod

=item B<replace_if_changed>

Currently undocumented.

=cut

sub replace_if_changed {
  my $original = shift;
  my $replacement = shift;

  # If the original doesn't exist, then move the replacement in.
  if (not -e $original)
  {
    move ($replacement, $original) or
      die "Failed to move '$replacement' to '$original'";
    return;
  }

  # If they are the same then nothing needs to be done.
  if (compare ($original, $replacement) == 0)
  {
    unlink $replacement or
      die "Failed to delete '$replacement': $!";
    return;
  }

  # Move the original to a backup, and then move in the new file.
  my $backup_base = $original.strftime ("_%Y-%m-%d_%H-%M", localtime ());
  my $backup = $backup_base;
  my $i = 2;
  while (-e $backup)
  {
    $backup = $backup_base . "_v$i";
    $i++;
    ($i < 10) or
      die "Couldn't find location to back '$original' too";
  }
  move ($original, $backup) or
    die "Couldn't move '$original' to '$backup': $!";
  move ($replacement, $original) or
    die "Couldn't move '$replacement' to '$original': $!";
  return;
}

#========================================================================#

=pod

=item B<find_partial_configs>

Currently undocumented.

=cut

sub find_partial_configs {
  my $config_dir = shift;
  my $hostname = shift;

  my @filenames;
  opendir my $dh, $config_dir or
    die "Failed to open '$config_dir': $!";

  while (readdir $dh)
  {
    next unless (m/^config\.(\d+)(?:\.([^.]+))?$/);

    my $for_host = $2;
    (defined $for_host) or $for_host = "all";
    my $number = $1;

    next if (($for_host ne "all")
               and ($for_host ne $hostname));

    if (defined ($filenames[$number]))
    {
      if ($filenames[$number]->{ -host } eq "all")
      {
        $filenames[$number] = undef;
      }
      elsif ($for_host eq "all")
      {
        next;
      }
      else
      {
        die "Filename '$_' appears to be duplicate";
      }
    }

    $filenames[$number] = { -filename => $_,
                            -path => abs_path ($config_dir."/$_"),
                            -host => $for_host,
                            -number => $number };
  }

  closedir $dh or
    die "Failed to close '$config_dir': $!";

  @filenames = grep { defined } @filenames;
  return @filenames;
}

#========================================================================#

=pod

=item B<main>

Currently undocumented.

=cut

sub main {
  my $name = hostname ();
  my $dir = abs_path (bsd_glob ($I3_CONFIG_DIRECTORY));
  (defined $dir) and (-d "$dir") or
    die "No directory: $I3_CONFIG_DIRECTORY\n";

  my ($fh, $filename) = tempfile (template => "temp.config.XXXXXXXX",
                                  dir => $dir);

  print $fh <<EOF
# i3 configuration file for host: $name

# i3 config file (v4)
#
# Please see https://i3wm.org/docs/userguide.html for a complete reference!

EOF
    ;

  foreach my $input_file (find_partial_configs ($dir, $name))
  {
    print $fh "## Number: ".$input_file->{ -number }."\n";
    print $fh "## Filename: ".$input_file->{ -filename }."\n";
    print $fh "## Host: ".$input_file->{ -host }."\n";
    print $fh "\n";

    open my $in, $input_file->{-path} or
      die "Failed to open '".$input_file->{-path}."': $!";

    while (<$in>)
    {
      print $fh $_;
    }

    close $in or
      die "Failed to close '".$input_file->{-path}."': $!";

    print $fh "\n";
  }

  close $fh or
    die "Failed to close `$filename`: $!";

  my $config_file = $dir."/config";
  replace_if_changed ($config_file, $filename);

  return 0;
}

#========================================================================#

#========================================================================#

=pod

=back

=head1 AUTHOR

Andrew Burgess, 11 Mar 2019

=cut
