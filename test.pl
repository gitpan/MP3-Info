#!/usr/bin/perl -w
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..91\n"; }
END {print "not ok 1\n" unless $loaded;}
use strict;
use MP3::Info 1.01;
use File::Copy;
use vars qw/$loaded/;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

my($tf1, $tf2, $tf3, $tf4, $tt1, $tt2, $ti1, $ti2,
    $ttd1, $ttd2, $tti1, $tti2, $c);
$c = 1;
$tf1 = 'test1.mp3';
$tf2 = 'test2.mp3';
$tf3 = 'test3.mp3';
$tf4 = 'test4.mp3';

@{$ttd1}{qw(ALBUM ARTIST GENRE COMMENT YEAR TITLE TRACKNUM)} = (
    '', 'Pudge', 'Sound Clip', 'All Rights Reserved',
    '1998', 'Test 1', 1
);

@{$ttd2}{qw(ALBUM ARTIST GENRE COMMENT YEAR TITLE TRACKNUM)} = (
    '', 'Pudge', 'Sound Clip', 'All Rights Reserved',
    '1998', 'Test 2', 2
);

@{$tti1}{qw(FREQUENCY STEREO BITRATE LAYER MM SS VERSION)} = (
    qw(44.1 1 128 3 0 0 1)
);

@{$tti2}{qw(FREQUENCY STEREO BITRATE LAYER MM SS VERSION)} = (
    qw(22.05 0 128 3 0 1 2)
);

# test 2
test($tt1 = get_mp3tag ($tf1), ++$c);
test($tt2 = get_mp3tag ($tf2), ++$c);
test($ti1 = get_mp3info($tf1), ++$c);
test($ti2 = get_mp3info($tf2), ++$c);

#test 6
for my $f (qw(ALBUM ARTIST GENRE COMMENT YEAR TITLE TRACKNUM)) {
    test_fields($tt1, $ttd1, $f);
    test_fields($tt2, $ttd2, $f);
}

# test 20
for my $f (qw(FREQUENCY STEREO BITRATE LAYER MM SS VERSION)) {
    test_fields($ti1, $tti1, $f);
    test_fields($ti2, $tti2, $f);
}

copy($tf1, $tf3) or die "Can't copy '$tf1' to '$tf3': $!";
copy($tf2, $tf4) or die "Can't copy '$tf2' to '$tf4': $!";

use_winamp_genres();

my %th = (ALBUM=>'hrmmm', ARTIST=>'hummmm', GENRE=>'Power Ballad');
while (my($k, $v) = each %th) {
    $tt1->{$k} = $ttd1->{$k} = $tt2->{$k} = $ttd2->{$k} = $v;
}

# test 34
test($tt1 = get_mp3tag ($tf3), ++$c);
test($tt2 = get_mp3tag ($tf4), ++$c);
test($ti1 = get_mp3info($tf3), ++$c);
test($ti2 = get_mp3info($tf4), ++$c);

# test 38
for my $f (qw(ALBUM ARTIST GENRE)) {
    test_fields($tt1, $ttd1, $f, 1);
    test_fields($tt2, $ttd2, $f, 1);
}

# test 44
for my $f (qw(FREQUENCY STEREO BITRATE LAYER MM SS VERSION)) {
    test_fields($ti1, $tti1, $f);
    test_fields($ti2, $tti2, $f);
}

# test 58
test(set_mp3tag($tf3, $ttd1), ++$c);
test(set_mp3tag($tf4, $ttd2), ++$c);

test($tt1 = get_mp3tag ($tf3), ++$c);
test($tt2 = get_mp3tag ($tf4), ++$c);
test($ti1 = get_mp3info($tf3), ++$c);
test($ti2 = get_mp3info($tf4), ++$c);

# test 64
for my $f (qw(ALBUM ARTIST GENRE COMMENT YEAR TITLE TRACKNUM)) {
    test_fields($tt1, $ttd1, $f);
    test_fields($tt2, $ttd2, $f);
}

# test 78
for my $f (qw(FREQUENCY STEREO BITRATE LAYER MM SS VERSION)) {
    test_fields($ti1, $tti1, $f);
    test_fields($ti2, $tti2, $f);
}

# test 92

unlink($tf3) or warn "Can't unlink '$tf3': $!";
unlink($tf4) or warn "Can't unlink '$tf4': $!";

sub test {
    print (($_[0] ? '' : 'not '), "ok $_[1]\n");
    return shift;
}

sub test_fields {
    my($f1, $f2, $f, $not) = @_;
    test(($not
            ? ($f1->{$f} ne $f2->{$f})
            : ($f1->{$f} eq $f2->{$f})), ++$c) ||
        printf "# wanted%s: $$f1{$f} (%d), got: $$f2{$f} (%d)\n",
            ($not ? ' not' : ''),
            length($f1->{$f}), length($f2->{$f});

}
