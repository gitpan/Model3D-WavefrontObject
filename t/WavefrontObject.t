
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl WavefrontObject.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

use Test;
use strict;

BEGIN {plan tests => 9};

eval {require Model3D::WavefrontObject};
ok ($@,'');
croak() if $@;
use Model3D::WavefrontObject;

# create model object #
my $model = Model3D::WavefrontObject->new;
ok ($model);

# Read in the included test object
$model->ReadObj('box.obj');

# Do some crap to it
ok (scalar $model->{v});
ok (scalar $model->{vt});
ok (scalar $model->{f});
ok ($model->Rotate(x => 45, y => 45, z => 45));
ok ($model->Translate(x => 0.01, y => 0.01, z => 0.01));
ok ($model->Scale('150%'));
ok ($model->Mirror('y'));

