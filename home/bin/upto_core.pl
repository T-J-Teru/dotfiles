#! /usr/bin/perl

use strict;
use warnings;

my $pattern = shift;
my $pwd = shift;

sub usage()
{
  print "Usage: $0 <pattern> <path>\n";
  die;
}

sub get_dest 
{
  # If we are in here: /xx/b/c/a/b/c
  # and use the pattern: a
  # we should print /xx/b/c/a/
  # If we use the pattern: xx/b
  # we should print /xx/b

  my $dir = shift;
  my $pattern = shift;
  $pattern =~ s,//+,/,g;

  $pattern = "^.*$pattern";

  if ( not ( $pattern =~ m,/$, ) )
  {
    $pattern = $pattern . "[^\/]*";
  }

  $dir =~ m/($pattern)/;

  my $res = ( (defined $1) ? $1 : "" );
  $res =~ s,/+$,,;
  return $res;
}


if ( exists $ENV{UPTO_CORE_TEST} )
{
  my $test_id = 1;
  sub test
  {
    my $src = shift;
    my $pattern = shift;
    my $dest = shift;
    
    my $res = get_dest($src, $pattern);
    printf("%6d ... ", $test_id);    
    if ( $res eq $dest )
    {
      printf("PASS\n");
    }
    else
    {
      printf("FAIL\n");
      printf("        -   source: $src\n");
      printf("        -  pattern: $pattern\n");
      printf("        - expected: $dest\n");
      printf("        -   result: $res\n");
    }
    $test_id++;
  }  
  
  test("/abc/def/ghi", "ef",  "/abc/def");
  test("/abc/def/ghi", "de",  "/abc/def");
  test("/abc/def/ghi", "def", "/abc/def");
  test("/abc/def/ghi", "c/d", "/abc/def");
  test("/abc/def/ghi", "/a",  "/abc"); 
  
  test("/abc/def/ghi", "ghi", "/abc/def/ghi");
  test("/abc/abc/abc", "abc", "/abc/abc/abc");
  test("/abc/abc/def", "abc/", "/abc/abc");
  test("/abc/abc/def", "b", "/abc/abc");
}
else
{
  defined $pattern or usage();
  defined $pwd or usage();
  
  print get_dest($pwd, $pattern);
}

