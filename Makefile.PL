use ExtUtils::MakeMaker;

WriteMakefile(
	'NAME'			=> 'MP3::Info',
	'VERSION_FROM'		=> 'lib/MP3/Info.pm',
	'PREREQ_PM'	=> {
    		'Test::More'	=> 0,
	},
	'clean'		=> {
		'FILES'		=> 'MP3-Info-*'
	},
	'NO_META'		=> 1,
);
