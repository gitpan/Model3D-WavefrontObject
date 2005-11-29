use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'Model3D::WavefrontObject',
    'VERSION_FROM'	=> 'WavefrontObject.pm',
    'PREREQ_PM'		=> {'Math::Trig' => '1.02'},
    'LIBS'		=> [''],
    'DEFINE'		=> '',
    'INC'		=> '',
    'AUTHOR'            => 'Sean \'Dodger\' Cannon (dodger@xfx3d.net)',
    'ABSTRACT'          => 'Alias Wavefront Object Read/Manipulate/Write',
);
