package Model3D::WavefrontObject;

use 5.006;
use strict;

our($VERSION);
$VERSION = 1.00;

use Math::Trig;

sub new {
    my $p = shift;
    my $class = ref $p || $p;
    my $obj = {};
    bless $obj, $class;
    $obj->_init(@_);
    return $obj;
}

sub _init {
    my $obj = shift;

    unless (scalar @_ % 2) {
        while (@_) {
            my $key = shift;
            $obj->{$key} = shift;
        }
    }

    $obj->{v} ||= [];
    $obj->{vt} ||= [];
    $obj->{vn} ||= [];
    $obj->{f} ||= [];
    $obj->{p} ||= [];
    $obj->{l} ||= [];
    $obj->{g} ||= {};
    $obj->{group} ||= {};
    $obj->{mtl} ||= {};
    $obj->{comments} ||= [];
    $obj->{r} ||= {};
    $obj->{_region} ||= 'none';
    $obj->{_material} ||= 'default';
    $obj->{_group} ||= 'NULL';

    if ($obj->{objfile}) {
        $obj->ReadObj($obj->{objfile});
    }

    return 1;
}

sub ReadObj {
    my $obj = shift;
    $obj->{objfile} = shift;
    unless ($obj->{objfile} =~ /\.obj$/ and $obj->{objfile}) {
        $obj->{objfile} .= '.obj';
    }
    unless (-e $obj->{objfile}) {
	return undef;
    }
    if (-d $obj->{objfile}) {
        $obj->{errstr} = "$obj->{objfile} is a directory.";
	return undef;
    }
    unless (-s $obj->{objfile}) {
        $obj->{errstr} = "$obj->{objfile}: File is zero size.";
	unless (-w $obj->{objfile}) {
	    $obj->{errstr} .= " Cannot modify file!";
	}
        return undef;
    }
    my $OBJ;
    unless (open ($OBJ, $obj->{objfile})) {
        $obj->{errstr} = "Can't read $obj->{objfile}: $!";
	return undef;
    }
    my $void = 1;
    my $vpid = 0;
    my $vtoid = 1;
    my $vnoid = 1;
    while (<$OBJ>) {
        chomp;
	s/\r//;
	s/^\s+//;
	s/\s+$//;
	if (/^v\s+/) { # Vertex line
	    /^v\s+([\d\+\-eE\.]+)\s+([\d\+\-eE\.]+)\s+([\d\+\-eE\.]+)\s*([\d\+\-eE\.]*)/;
	    my $x = $1 + 0;
	    my $y = $2 + 0;
	    my $z = $3 + 0;
	    my $wt = $4 + 0 || 1;
	    push @{$obj->{v}}, {x => $x,
	                        y => $y,
				z => $z,
				wt => $wt,
				id => $void,
				pid => $vpid};
	    $void++;
	    $vpid++;
	}
	elsif (/^#\s*r\s+(.*)$/) { # UVMapper Region Extension
	    push @{$obj->{r}}, $1;
	    $obj->{r}->{$1} = 1;
	    $obj->{_region} = $1;
	}
	elsif (/^g$/) { # NULL group declaration
	    my $group = 'NULL';
	    $obj->{g}->{NULL} = 1;
	    $obj->{seengroup}->{$group} = 1;
	    $obj->{_group} = $group;
	}
	elsif (/^g\s+(\S*)$/) { # named group declaration
	    my $group = $1;
	    $group = 'NULL' if lc $group eq '(null)';
	    $obj->{g}->{$group} = 1;
	    $obj->{_group} = $group;
	}
	elsif (/^usemtl\s*(\S*)/) { # Material declaration
	    my $material = $1;
	    $obj->{mtl}->{$material} = '';
	    $obj->{_material} = $material;
	}
	elsif (/^mtllib\s+(.*)\s*$/) { # declare material library
	    my $mtllib = $1;
	    $mtllib =~ s/[\\\:]/\//g;
	    $obj->{mtllib} = $mtllib;
	    $obj->ReadMtlLib;
	}
	elsif (/^vt\s+/) { # UV/UVW line
	    /^vt\s+([\d\+\-eE\.]+)\s+([\d\+\-eE\.]+)\s*([\d\+\-eE\.]*)/;
	    my $u = $1 + 0;
	    my $v = $2 + 0;
	    my $w = $3 + 0;
	    push @{$obj->{vt}}, {u => $u,
	                         v => $v,
				 w => $w,
				 id => $vtoid};
	    $vtoid++;
	}
	elsif (/^vn\s+/) {
	    /^vn\s+([\d\+\-eE\.]+)\s+([\d\+\-eE\.]+)\s+([\d\+\-eE\.]+)/;
	    my $i = $1 + 0;
            my $j = $2 + 0;
	    my $k = $2 + 0;
	    push @{$obj->{vn}}, {i => $i,
	                         j => $j,
				 k => $k,
				 id => $vnoid};
	    $vnoid++;
	}
	elsif (/^fo?\s+(.*)$/) { # Polygon line
	    my $p = $1;
	    my @poly = split " ", $p;
	    my @p;
            for my $pv (@poly) {
	        my ($v, $vt, $vn) = split /\//, $pv;
		# OBJ files are 1-indexed. We want the right element
		# BUT counting backwards is as we expect.
		$v-- if $v > 0;
		$vt-- if $vt > 0;
		$vn-- if $vn > 0;
                push @p, {v => $obj->{v}->[$v],
		          vt => $obj->{vt}->[$vt],
			  vn => $obj->{vn}->[$vn],
			  g => $obj->{_group},
			  m => $obj->{_material},
			  r => $obj->{_region}};
	        push @{$obj->{group}->{$obj->{_group}}},
		       {v => $obj->{v}->[$v],
                        vt => $obj->{vt}->[$vt],
                        vn => $obj->{vn}->[$vn],
                        m => $obj->{_material},
			r => $obj->{_region}}
		  unless $obj->{seengroupv}->{$obj->{_group}}->{$v};
		$obj->{seengroupv}->{$obj->{_group}}->{$v} = 1;
	    }
	    push @{$obj->{f}}, {verts => \@p,
	                        group => $obj->{_group},
				material => $obj->{_material},
				region => $obj->{_region}};

	    # Theoretically, you can now get the x, y, and z coordinates and
	    # UV coordinates and group and material for, say, the third vertex
	    # in the 9th facet like so:

	    #     $x = $obj->{f}->[10]->{verts}->[2]->{v}->{x};
	    #     $y = $obj->{f}->[10]->{verts}->[2]->{v}->{y};
	    #     $z = $obj->{f}->[10]->{verts}->[2]->{v}->{z};
	    #     $g = $obj->{f}->[10]->{group};
	    #     $m = $obj->{f}->[10]->{material};
	    #     $u = $obj->{f}->[10]->{verts}->[2]->{vt}->{u};
	    #     $v = $obj->{f}->[10]->{verts}->[2]->{vt}->{v};

            # Or, to make it even easier:

	    #     $fv = $obj->{f}->[10]->{verts}->[2];
	    #     $y = $fv->{v}->{y};
	    #     $u = $fv->{vt}->{u};
	}
	elsif (/^l\s+(.*)$/) { # Line line
	    my $l = $1;
	    my @line = split " ", $l;
            my @l;
	    for my $lv (@line) {
	        my ($v, $vt) = split /\//, $lv;
		$v-- if $v > 0;
                $vt-- if $vt > 0;
		push @l, {v => $obj->{v}->[$v],
		          vt => $obj->{vt}->[$vt]};
	    }
	    push @{$obj->{l}}, \@l;
	}
	elsif (/^p\s+(.*)$/) { # Point line
	    my $v = $1;
	    $v-- if $v > 0;
	    push @{$obj->{p}}, $v;
	}
	elsif (/^\s*#\s*(.*)$/) { # comment
	    push @{$obj->{comments}}, $1;
	}
    }
    close $OBJ;
    return 1;
}

sub ReadMtlLib {
    my $obj = shift;
    my $mtllib = shift;
    $obj->{mtllib} = $mtllib;
    return undef unless $mtllib;
    my $MTL;
    unless (open ($MTL, "$mtllib")) {
        $obj->{errstr} = "Can't read material library $mtllib.";
	return undef;
    }
    while (<$MTL>) {
        chomp;
	s/\r//;
	s/^\s+//;
	s/\s+$//;
	if (/^newmtl\s+(\S+)/) {
	    $obj->{_defmtl} = $1;
	}
	elsif (/^Ka\s+([\d\.eE\-\+]+)\s+([[\d\.eE\-\+]+)\s+([[\d\.eE\-\+]+)/) {
	    my $r = $1;
	    my $g = $2;
	    my $b = $3;
	    $obj->{mtl}->{$obj->{_defmtl}}->{Ka}->{r} = $r * 255;
	    $obj->{mtl}->{$obj->{_defmtl}}->{Ka}->{g} = $g * 255;
	    $obj->{mtl}->{$obj->{_defmtl}}->{Ka}->{b} = $b * 255;
	}
	elsif (/^Kd\s+([\d\.eE\-\+]+)\s+([[\d\.eE\-\+]+)\s+([[\d\.eE\-\+]+)/) {
	    my $r = $1;
	    my $g = $2;
	    my $b = $3;
	    $obj->{mtl}->{$obj->{_defmtl}}->{Kd}->{r} = $r * 255;
	    $obj->{mtl}->{$obj->{_defmtl}}->{Kd}->{g} = $g * 255;
            $obj->{mtl}->{$obj->{_defmtl}}->{Kd}->{b} = $b * 255;
	}
	elsif (/^Ks\s+([\d\.eE\-\+]+)\s+([[\d\.eE\-\+]+)\s+([[\d\.eE\-\+]+)/) {
	    my $r = $1;
            my $g = $2;
            my $b = $3;
            $obj->{mtl}->{$obj->{_defmtl}}->{Ks}->{r} = $r * 255;
	    $obj->{mtl}->{$obj->{_defmtl}}->{Ks}->{g} = $g * 255;
	    $obj->{mtl}->{$obj->{_defmtl}}->{Ks}->{b} = $b * 255;
	}
	elsif (/^illum\s+(\d)/) {
	    $obj->{mtl}->{$obj->{_defmtl}}->{illum} = $1 + 0;
	}
	elsif (/^Ns\s+([\d\.eE\-\+]+)/) {
	    $obj->{mtl}->{$obj->{_defmtl}}->{Ns} = $1 + 0;
	}
	elsif (/^(d|Tr)\s+([\d\.eE\-\+]+)/) {
	    $obj->{mtl}->{$obj->{_defmtl}}->{Tr} = $1 + 0;
	}
	elsif (/^map_Ka\s+(.*)/) {
	    $obj->{mtl}->{$obj->{_defmtl}}->{textureMap} = $1;
	}
    }
    close $MTL;
    return 1;
}

sub Translate {
    my $obj = shift;
    my $trans;
    while (@_) {
        my $axis = shift;
	my $amount = shift;
        $trans->{$axis} = $amount + 0;
    }
    for my $v (@{$obj->{v}}) {
        if ($trans->{x}) {
	    $v->{x} += $trans->{x};
	}
	if ($trans->{y}) {
	    $v->{y} += $trans->{y};
	}
	if ($trans->{z}) {
	    $v->{z} += $trans->{z};
	}
    }
    return 1;
}

sub _getTransCentre {
    my $obj = shift;
    my $centre = {x => 0,
                  y => 0,
		  z => 0};
    my $c = shift;
    return $centre unless $c;
    return $obj->GetNaturalCentre if $c eq 'natural';
    return $obj->GetApparentCentre if $c eq 'apparent';
    my @stdrot = qw(x y z);
    if (ref $c and ref $c ne 'SCALAR') {
        # They can use an arrayref like center => [x,y,z]
	if (ref $c eq 'ARRAY') {
	    for my $p (@{$c}) {
	        my $ax = shift @stdrot;
		$centre->{$ax} = $p;
	    }
	}
	# ...or a hashref like center => {x => x, y => y, z => z}
	elsif (ref $c eq 'HASH') {
	    for my $k (keys %{$c}) {
	        $centre->{$k} = $c->{$k};
            }
	}
    }
    else {
        # Or a scalarref like center => \$center
	if (ref $c and ref $c eq 'SCALAR') {
	    $c = ${$c};
	}
	# or a real scalar in two ways:
	$c =~ s/\s+//g; # (ignoring whitespace)
	my @c = split /,/, $c;
	for my $p (@c) {
	    my ($ax, $r);
	    # Either like 'x:x,y:y,z:z'
	    if ($p =~ /:/) {
	        ($ax, $r) = split /:/, $p;
            }
	    # ...or like 'x,y,z'
	    else {
	        $ax = shift @stdrot;
		$r = $p;
	    }
	    $centre->{$ax} = $r;
	}
    }
    return $centre;
}

sub GetNaturalCentre {
    my $obj = shift;
    my $vcount = scalar @{$obj->{v}};
    my $centre = {x => 0,
                  y => 0,
		  z => 0};
    return $centre unless $vcount;
    my ($x, $y, $z) = (0,0,0);
    for my $v (@{$obj->{v}}) {
        $x += $v->{x};
	$y += $v->{y};
	$z += $v->{z};
    }
    $centre->{x} = $x / $vcount;
    $centre->{y} = $y / $vcount;
    $centre->{z} = $z / $vcount;
    return $centre;
}

sub GetApparentCentre {
    my $obj = shift;
    my $center = {x => 0,
                  y => 0,
		  z => 0};
    return $center unless scalar @{$obj->{v}};
    my ($min, $max) = $obj->MinMax;
    $center->{x} = $max->{x} + $min->{x} / 2;
    $center->{y} = $max->{y} + $min->{y} / 2;
    $center->{z} = $max->{z} + $min->{z} / 2;
    return $center;
}

sub MinMax {
    my $obj = shift;
    my $max = {x => 0,
               y => 0,
               z => 0};
    my $min = {x => 0,
               y => 0,
               z => 0};
    return ($min, $max) unless scalar @{$obj->{v}};
    for my $v (@{$obj->{v}}) {
        $max->{x} = $v->{x} if $v->{x} > $max->{x};
        $min->{x} = $v->{x} if $v->{x} < $min->{x};
        $max->{y} = $v->{y} if $v->{y} > $max->{y};
        $min->{y} = $v->{y} if $v->{y} < $min->{y};
        $max->{z} = $v->{z} if $v->{z} > $max->{z};
        $min->{z} = $v->{z} if $v->{z} < $min->{z};
    }
    return ($min, $max);
}

sub Top {
    my $obj = shift;
    my ($min, $max) = $obj->MinMax;
    return $max->{y};
}

sub Bottom {
    my $obj = shift;
    my ($min, $max) = $obj->MinMax;
    return $min->{y};
}

sub Left {
    my $obj = shift;
    my ($min, $max) = $obj->MinMax;
    return $min->{x};
}

sub Right {
    my $obj = shift;
    my ($min, $max) = $obj->MinMax;
    return $max->{x};
}

sub Front {
    my $obj = shift;
    my ($min, $max) = $obj->MinMax;
    return $max->{z};
}

sub Back {
    my $obj = shift;
    my ($min, $max) = $obj->MinMax;
    return $min->{z};
}

sub ReverseWinding {
    my $obj = shift;
    unless (scalar @{$obj->{f}}) {
        $obj->{errstr} = 'This object has no facet information';
	return undef;
    }
    for my $f (@{$obj->{f}}) {
        $f->{verts} = [reverse @{$f->{verts}}];
    }
    return 1;
}

sub Rotate {
    my $obj = shift;
    my $rot;
    while (@_) {
        my $axis = shift;
	my $amount = shift;
	$rot->{$axis} = $amount + 0;
    }
    my $centre = $obj->_getTransCentre($rot->{centre} || $rot->{center});
    return undef unless $rot;
    for my $v (@{$obj->{v}}) {
	if ($centre->{x} || $centre->{y} || $centre->{z}) {
	    if ($centre->{x}) {
	        $v->{x} -= $centre->{x};
	    }
	    if ($centre->{y}) {
	        $v->{y} -= $centre->{y};
	    }
	    if ($centre->{z}) {
	        $v->{z} -= $centre->{z};
	    }
	}
        if ($rot->{x}) {
	    my $rad = Math::Trig::deg2rad($rot->{x});
            my ($rho, $theta, $phi)
	      = Math::Trig::cartesian_to_spherical($v->{y}, $v->{z}, 0);
	    $theta += $rad;
	    ($v->{y}, $v->{z}, undef)
	      = Math::Trig::spherical_to_cartesian($rho, $theta, $phi);
	}
	if ($rot->{y}) {
	    my $rad = Math::Trig::deg2rad($rot->{y});
	    my ($rho, $theta, $phi)
	      = Math::Trig::cartesian_to_spherical($v->{x}, $v->{z}, 0);
	    $theta += $rad;
	    ($v->{x}, $v->{z}, undef)
	      = Math::Trig::spherical_to_cartesian($rho, $theta, $phi);
	}
	if ($rot->{z}) {
	    my $rad = Math::Trig::deg2rad($rot->{z});
	    my ($rho, $theta, $phi)
	      = Math::Trig::cartesian_to_spherical($v->{x}, $v->{y}, 0);
	    $theta += $rad;
	    ($v->{x}, $v->{y}, undef)
	      = Math::Trig::spherical_to_cartesian($rho, $theta, $phi);
	}
        if ($centre->{x} || $centre->{y} || $centre->{z}) {
            if ($centre->{x}) {
                $v->{x} += $centre->{x};
            }
            if ($centre->{y}) {
                $v->{y} += $centre->{y};
            }
            if ($centre->{z}) {
                $v->{z} += $centre->{z};
            }
        }
    }
    return 1;
}

sub _getScaleVal {
    my $obj= shift;
    my $sv = shift;
    return 1 unless $sv;
    my $op;
    $sv =~ s/([\+\-])//;
    $op = $1;
    if ($sv =~ s/\%$//) {
        $sv /= 100;
    }
    if ($op) {
        if ($op eq '-') {
	    $sv = 1 - $sv;
	}
	elsif ($op eq '+') {
	    $sv = 1 + $sv;
	}
    }
    return $sv;
}

sub Scale {
    my $obj = shift;
    my $scale = {x => 1,
                 y => 1,
		 z => 1};
    if (scalar @_ > 1) {
        while (@_) {
            my $axis = shift;
            my $amount = $obj->_getScaleVal(shift);
            $scale->{$axis} = $amount;
        }
    }
    else {
        my $s = shift;
	$scale->{x} = $obj->_getScaleVal($s);
	$scale->{y} = $obj->_getScaleVal($s);
	$scale->{z} = $obj->_getScaleVal($s);
    }
    if ($scale->{scale}) {
        $scale->{scale} = $obj->_getScaleVal($scale->{scale});
        $scale->{x} ||= $scale->{scale};
	$scale->{y} ||= $scale->{scale};
	$scale->{z} ||= $scale->{scale};
    }
    my $centre = $obj->_getTransCentre($scale->{centre} || $scale->{center});
    for my $v (@{$obj->{v}}) {
        $v->{x} *= $scale->{x};
	$v->{y} *= $scale->{y};
	$v->{z} *= $scale->{z};
    }
    return 1;
}

sub GetVertex {
    my $obj = shift;
    my $vert = shift;
    unless ($vert) {
        $obj->{errstr} = 'No vertex specified';
	return undef;
    }
    unless (exists $obj->{v}->[$vert]) {
        $obj->{errstr} = "Vertex $vert does not exist";
	return undef;
    }
    return wantarray ? ($obj->{v}->[$vert]->{x},
                        $obj->{v}->[$vert]->{y},
			$obj->{v}->[$vert]->{z})
		     : $obj->{v}->[$vert];
}

sub GetVertexSpherical {
    my $obj = shift;
    my $vert = shift;
    unless ($vert) {
        $obj->{errstr} = 'No vertex specified';
        return undef;
    }
    unless (exists $obj->{v}->[$vert]) {
        $obj->{errstr} = "Vertex $vert does not exist";
        return undef;
    }
    my $v = $obj->{v}->[$vert];
    my ($rho, $theta, $phi)
     = Math::Trig::cartesian_to_spherical($v->{x}, $v->{y}, $obj->{z});
    $theta = Math::Trig::rad2deg($theta);
    $phi = Math::Trig::rad2deg($phi);
    return wantarray ? ($rho, $theta, $phi) : {rho => $rho,
                                               theta => $theta,
					       phi => $phi};
}

sub Mirror {
    my $obj = shift;
    my $ax = shift;
    $ax ||= 'x';
    for my $v (@{$obj->{v}}) {
        $v->{$ax} = 0 - $v->{ax};
    }
    $obj->ReverseWinding;
    return 1;
}

sub FlipUVs {
    my $obj = shift;
    my $ax = shift;
    $ax ||= 'u';
    for my $vt (@{$obj->{vt}}) {
        $vt->{$ax} = 1 - $vt->{$ax};
    }
    return 1;
}

sub WriteObj {
    my $obj = shift;
    my $outfile = shift || $obj->{outfile};
    $obj->{outfile} = $outfile;

    my $OBJ;
    my $was_stdout = 0;
    if ($obj->{outfile}) {
        open $OBJ, ">$obj->{outfile}";
    }
    else {
        $was_stdout = 1;
        $OBJ = *STDOUT{IO};
    }

    my $prec = $obj->{prec} || 8;

    # Print out file comments
    unshift @{$obj->{comments}},
            "File generated by MapShape.pl (c) Dodger",
	    scalar @{$obj->{v}} . ' Vertices',
	    scalar @{$obj->{vt}} . ' UVs',
	    scalar @{$obj->{f}} . ' Polygons',
	    scalar(keys(%{$obj->{g}})) . ' Groups',
	    scalar(keys(%{$obj->{mtl}})) . 'Materials',
	    scalar(keys(%{$obj->{r}})) . 'Regions';
    for my $comment (@{$obj->{comments}}) {
        print {$OBJ} "# $comment\n";
    }
    print {$OBJ} "\n";

    # Print out vertices
    for my $v (@{$obj->{v}}) {
        my $pf = "v %.${prec}f %.${prec}f %.${prec}f\n";
        printf {$OBJ} $pf, $v->{x}, $v->{y}, $v->{z};
    }
    print {$OBJ} "\n";

    # Print out UVs
    for my $vt (@{$obj->{vt}}) {
        printf {$OBJ} "vt %f %f %f\n", $vt->{u}, $vt->{v}, $vt->{w};
    }
    print {$OBJ} "\n";

    # This is for Poser for now, so no normals.
    # We bailed unless we had UVs, so we assume we have them.
    # There is a slight chance that a model has SOME UVs but not all.
    # Fuck that noise. That's a fucked up improper model, and just rude
    # to do. We're not covering that screwy contingency.

    my ($r, $g, $m);
    for my $f (@{$obj->{f}}) {
        if ($r ne $f->{region}) {
	    $r = $f->{region};
	    print {$OBJ} "# r $r\n";
	}
	if ($g ne $f->{group}) {
	    $g = $f->{group};
	    print {$OBJ} "g $g\n";
	}
	if ($m ne $f->{material}) {
	    $m = $f->{material};
	    print {$OBJ} "usemtl $m\n";
	}
        my $outpoly = join " ",
	                   map "$_->{v}->{id}/$_->{vt}->{id}",
			       @{$f->{verts}};
        print {$OBJ} "f $outpoly\n";
    }
    close $OBJ unless $was_stdout;
    return 1;
}

1;

__END__

=head1 NAME

Model3D::WavefrontObject - Perl extension for reading, manipulating and writing polygonal Alias Wavefront 3D models

=head1 SYNOPSIS

      use Model3D::WavefrontObject;
      my $model = Model3D::WavefrontObject->new;
      $model->ReadObj('blMilWom_v3.obj');
      $model->Rotate(x => 45, y => 15);
      $model->Scale(135%);
      $model->WriteObj('V3_modified.obj');

=head1 DESCRIPTION

Model3D::WavefrontObject allows a polygonal Alias Wavefront Object file to be
loaded into memory, manipulated, analysed, and re-output.

At this time the model only supports the polygon functions of the Wavefront
Object format, not bezier splines, procedural surfaces, and so on. It is
currently coded only far enough to support the sorts of Wavefront Object
meshes that are also supported by the Poser 3D animation program, and of the
sort exported by 3DSMax by using the HABWare exporter.

It supports groups and materials, but not multi-grouped polygons (only the
first group is recognised if a polygon is declared to be in multiple groups).

Polygons with greater numbers of vertices are supported, even though these
are not supported by the Poser software.

The models will also recognise (and support) the Region extension to the format
as defined by Steve Cox's UVMapper and UVMapper Pro program. As a result, it
may well be the only piece of code that writes Wavefront Objects without
leaving these out.

=head1 METHODS

=head2 Constructor

    new()

The C<new()> method returns a new Model3D::WavefrontObject object.

You may additionally supply other parametres to the constructor, including,
if you so choose, all the data needed to construct an object (following the
format I<of> the object as shown in B<PROPERTIES>, below).

The most common parametre to send into the constructor would probably be the
C<objfile> parametre. This can be set either to a file path or name, or to a
reference to a filehandle. If this is done, the C<ReadObj()> method will be
called automatically by the internal C<_init()> method when the object is
created and before it's returned. Uhm, that is to say, you don't need to call
it yourself.

It all depends on how specific you want to be.

=head2 Public IO Methods

=head4 ReadObj(filehandle or filename)

Use the C<ReadObj()> method to read a Wavefront Object file into memory. Unless
you are building one from scratch, you'll most likely want to do this.

You can provide a filename (either absolute or relative path, which means that
both
C<'C:\Program Files\Curious Labs\Poser 4\Runtime\Geometries\XFXPeople\grpAeonTeenF.obj'>
and C<'../whatever/someObj.obj'> will work.

It should be noted that Poser-relative paths will NOT work at this time. I.e.,
the string ':Runtime:Geometries:XFXCritters:grpChickenThing.obj' will I<not> be
parsed. This may change in a future release, but right now it's a bit tricky
to tell whether the path is a relative one, or a path on an older Macintosh, as
both have the same directory seperators (for reasons obvious to those familiar
with the history of Poser).

Returns 1 on success, C<undef> on failure. Check C<{errstr}> for why.

=head4 WriteObj(outfile)

Writes the Wavefront Object to C<outfile>. If no outfile is provided, writes to
C<STDOUT>.

C<WriteObj()> can accept either a filename or a filehandle reference, as
C<ReadObj()>, above. The difference is that C<WriteObj()> doesn't require the
file in question already exist.

Returns 1 on success, C<undef> on failure. Check C<{errstr}> for why.

=head4 ReadMtlLib(mtllib)

The C<ReadMtlLib()> method reads a material library into memory and associates
the material lib data with the materials defined in the object mesh. If,
while reading the Wavefront Obj, a C<mtllib> directive is encountered, this
method is automatically called (though in experience, it will rarely actually
find the lib, as people almost never provide it with).

=head2 Public Manipulation Methods

=head4 Translate(translations)

The C<Translate()> method moves the object to the left, right, up, down, and/or
forwards and back. Specify the translations you want to perform when you call
the method in hash format. For instance:
    $model->Translate(x => .1) # Translates 0.1 units to the object's left.
    $model->Translate(x => -0.3) # Translates -0.3 units to the object's right.
    $model->Translate(y => 2) # Translates the model 2 units upward.
    $model->Translate(z => -5) # Translates the model 5 units back.

=head4 Rotate(rotations, optional centre)

The C<Rotate()> method rotates the object. You specify the rotations per axis
in hash format, i.e. C<{x =E<gt> val, y =E<gt> val, z =E<gt> val}>.

You may also specify an optional centre to perform the rotations around, by
setting a C<centre =E<gt>> property when you call the method. This centre
may be specified with a hashref (C<{x =E<gt> val, y =E<gt> val, z =E<gt> val}>),
an arrayref (C<[x, y, z]>), or a string in two formats: either C<x,y,z> or
C<x:val,y:val,z:val>. A scalar reference to such a string is also acceptable.

Additionally, the strings 'natural' and 'apparent' can be provided. If the
centre is specified as 'apparent', the centre will be positioned in the centre
of the bounding box that would surround the object. If 'natural' is specified,
the centre will be positioned at the absolute average co-ordinate of the object.
I.e., the average Y position of all of the vertices, and so on. This will cause
objects that have, for instance, a heavy concentration of smaller,
higher-resolution polygons in one location to have a centre closer to that
concentration.

Note that if the arrayref method or the C<x,y,z> string method is used, the
order cannot be specified; it will always be in X,Y,Z order. However, as the
rotation actually modifies the object and is not local to the object's axis,
the rotations are not subject to gimbal lock. The order of rotation will,
however affect the result, moreso the further the centre is from the actual
object.

As a concession to the rebel colonies, the centre may also be supplied with
the popular but improper spelling of 'center'.

=head4 Scale(scale/scales, optional centre)

The C<Scale()> method scales the object. You can specify the scales in hash
format, as per C<Rotate()> and C<Translate()> above, or as a single value
which will be applied to all three axes. You may also supply a centre, as in
C<Rotate()>, above.

Since it is conceivable that you don't want to specify all three scales, but
you do want to specify a centre, and since the one-argument method above
does not provide for the option of a centre to be specified (since it needs
to be just one argument), you may also provide an argument of C<scale>, which
will suffice as the scale to apply to all three. As a side effect of this
approach, you can specify a single axis for one scaling factor, as per the
normal hash format above (C<x =E<gt> 110%>), and then use the centre argument
to specify both the other two at once.

You may specify the scaling factor as an absolute factor of 1 (i.e., .9,
1.7, and so on), as a percentage factor (90%, 170%), or as a relative amount
(-.1, +.7). You may also combine relative and percentage approaches (-10%,
+70%).

Any axis not specified defaults to a factor of '1', not 0, which means that
unspecified axes do not flatten along that axis, as this would be unbearably
annoying.

=head4 Mirror(x||y||z)

The C<Mirror()> method causes an object to be mirrored along the axis specified.
For instance, if you Mirror along the X axis, translate forward along the z
axis, and rotate 180 degrees on the y axis, you will get a version of the figure
that would be looking out of a mirror at the original figure.

C<Mirror()> does not mirror UVW coordinates. To do that, call C<FlipUVs>
(see below). Note that this is important to know, as depending on the method
used to interpret standard bumpmaps, and always in the case of Poser 4
'greenscale' bumpmaps, if an object is mirrored but the UVs and images used as
maps are not inverted, the effect of the bumpmap will be inverted (causing 
white to indicate low areas and black to indicate high areas).

It should be additionally noted that mirroring the contents of a P4 BUM bumpmap
will NOT work. You need to invert the greyscale bumpmap, then convert *that* to
a greenscale bumpmap.

If no axis is specified, X is used.

=head4 FlipUVs('u' or 'v')

This method flips the UVs of the model. If 'v' is specified, the UVs are flipped
vertically. If 'u' is specified, or if no axis is specified, the UVs are flipped
horizontally.

=head4 ReverseWinding

The C<ReverseWinding()> method reverses the winding order of the polygons'
vertices. In effect (and in Poser), this reverses the normals of those
polygons, effectively flipping a model inside-out or outside-in. The C<Mirror()>
method automatically calls this method after mirroring, to preserve the original
appearance of the surfaces.

Note that Poser 4 and ProPack, and the Poser renderer in Poser 5 and 6 do not
pay any attention to the normals and treat all polygons as 2-sided. It does,
however, affect the preview mode, as well as affecting P5/6 Firefly renders by
default.

Object files exported from ZBrush will usually have inverted normals because
ZBrush inverts the Z coordinate internally (if you've used ZBrush and imported
an Object, you've most likely noticed that you had to turn it around after
loading it into your canvas). This method will fix that handily.

=head2 Utility Methods

=head4 GetVertex(id)

This method returns a hashref containing the x, y, and z coordinates of the
specified vertex, as well as the id and 'pid' (Poser ID) of it.

The vertex specified is the 0-indexed vertex. It should be noted that this is
I<not> the id as given in any given polygon (f) specification in an Alias
Wavefront Object file, as the file format uses an index od 1 rather than 0.

The vertex retrived will have an C<id> property set to the 1-indexed value,
as well as a C<pid> property set to the 0-indexed value. 'pid' stands for
Poser ID, as Poser refers to vertices by their 0-indexed elements (i.e., in
a C<TargetGeom delta> directive in a Poser file).

If you want to select by the ID that the file format itself uses, simply
subtract one from the value you want to specify:
    my $vert = $model-E<gt>GetVertex($vertid - 1);

=head4 GetVertexSpherical(id)

This method works just like C<GetVertex()>, above, except that it returns
the I<spherical co-ordinates> of the vertex requested, in the order: Radial
(rho), Azimuthal (theta), Polar (phi).

=head4 GetNaturalCentre

This method returns the natural centre of the model, in the form of a hashref
containing the keys x,y and z with those values applied. If the model does not
have any vertices when the method is called, the values are all 0.

The natural centre is the average x, y and z coordinate of all vertices, as
explained in C<Rotate()>, above.

This method is not spelled in American.

=head4 GetApparentCentre

This method returns the apparent centre of the model, in the form of a hashref
as with C<GetNaturalCentre>, above. The difference, as explained under the
C<Rotate()> method, is that the apparent centre is the midway point between the
top, bottom, left, right, front anc back vertices.

=head4 MinMax

This method, primarily used internally but made to be accessible publically as
well, returns two variables. The first is a hashref containing the minimum x,
y and z co-ordinates, and the second is a hashref containing the maximum x,
y and z co-ordinates of the model.

These values can be used to construct a bounding box, of course.

Because it's only meant to be called in a list context, if called in a scalar
context the method will always return C<2>. Don't use it like that.

=head4 Top

This method returns the highest Y value in the model.

=head4 Bottom

This method returns the lowest Y value in the model.

=head4 Left

This method returns the highest (leftmost) X value in the model.

=head4 Right

This method returns the lowest (rightmost) X value in the model.

=head4 Front

This method returns the highest (foremost) Z value in the model.

=head4 Back

This method returns the highest (rearmost) Z value in the model.

=head2 Private Methods

There are (currently) three private methods, _getScaleVal, _getTransCentre,
and _init. These are only for internal convenience parsing and are very
unlikely to do an API user any good, so leave them alone. If you must know,
read the source. It hasn't been bleached or eyedropped or anything.

=head1 AUTHOR

Sean 'Dodger' Cannon  qbqtre@ksk3q.arg =~ tr/a-mn-z/n-za-m/
L<http://www.xfx3d.net>

=head1 BUGS

=over

=item * Does not handle beziers/splines

=item * Does not handle procedural geometry

=item * Does not save normals out (but it does read them)

=item * Does not yet write out MTLLibs

=back

=head1 GOOD INTENTIONS

=over

=item * A Clone() method that duplicates the object without the innards being references to the first innards (allowing seperate manipulation of each)

=item * Figure out how to handle beziers/splines and poly-fy them

=item * Figure out how to handle procedural geometry and poly-fy it

=item * Save out normals

=item * Save out mtllibs (material libraries)

=item * Apply morphs by group, material, or region

=item * Extract geometry by group

=item * Extract geometry by material

=item * Extract geometry by region

=item * Insert/append geometry

=item * Increase mesh resolution (all or by group, material, region)

=item * Project UVWs from one model onto another (I have an idea...)

=item * Decrease mesh resolution. Dream on.

=item * Apply boolean functions to geometry. Hahahaha! Yeah right! *splutter*

=item * Interface with other 3D model modules. So far I think VMRL is all there is

=item * Build in several utility scripts I have (as file processors) as methods

=item * Feed the cat

=back

=head1 SEE ALSO

perl(1)

Model3D::Poser (coming soon)

=cut

