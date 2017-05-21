#!/usr/bin/perl -w

use strict;
use Carp;
use FindBin;

my $utilDir = $FindBin::Bin;
my $rootDir = "$utilDir/..";
my $apiDir = "$rootDir/cbits/api";
my $glueDir = "$rootDir/cbits/glue";
my $outputDir = "$rootDir/src/System/Hardware/MercuryApi";
my $generatedFile = "$outputDir/Generated.hsc";
my $paramFile = "$outputDir/Params.hs";

my @errorTypes = ("SUCCESS_TYPE");
my @errorCodes = ("SUCCESS");
my %errorCodes = ("SUCCESS" => "Success!  (Never thrown in an exception)");
my @glueTypes = ();
my %glueTypes = ();
my @glueCodes = ();
my %glueCodes = ();

my @params = ();
my %params = ();
my %paramType = ();
my %paramPath = ();
my %paramReadOnly = ();

my @regions = ();
my %regions = ();
my %regionsUnescaped = ();

my @tagProtocols = ();

my @metadataFlags = ();

my @banks = ();
my %banks = ();

my @lockBits = ();
my %lockBits = ();

my %tagDataStructs = ();
my %gen2Structs = ();
my %gpioStructs = ();
my %tagopStructs = ();

my @lines = ();

my $nyi = "(Not yet implemented)";

# Additional statuses of type ERROR_TYPE_BINDING.  These statuses are thrown
# only from Haskell code, not C code, so they don't have a numeric code.
my %haskellCodes = (
    "ERROR_UNIMPLEMENTED_PARAM" =>
    "The given parameter is not yet implemented in the Haskell binding.",
    "ERROR_INVALID_PARAM_TYPE" =>
    "The parameter value was not of the type expected."
    );

# toHaskellType should only contain types needed as param types
my %toHaskellType = (
    "bool"     => "Bool",
    "int8_t"   => "Int8",
    "int16_t"  => "Int16",
    "int32_t"  => "Int32",
    "uint8_t"  => "Word8",
    "uint16_t" => "Word16",
    "uint32_t" => "Word32",
    "TMR_Region" => "Region",
    "TMR_TagProtocol" => "TagProtocol",
    "TMR_ReadPlan"    => "ReadPlan",
    "TMR_GEN2_Password" => "Word32",
    "TMR_TRD_MetadataFlag" => "[MetadataFlag]",
    "TMR_String" => "Text",
    "TMR_uint8List"  => "[Word8]",
  # "TMR_uint16List" => "[Word16]",
    "TMR_uint32List" => "[Word32]",
  # "TMR_int8List"   => "[Int8]",
    "TMR_RegionList" => "[Region]",
    "TMR_TagProtocolList" => "[TagProtocol]",
    );

# toHaskellType2 may contain other types not needed as param types
my %toHaskellType2 = (
    %toHaskellType,
    "TMR_GEN2_TagData" => "GEN2_TagData",
    "TMR_TagData" => "TagData",
    "TMR_GEN2_Bank" => "GEN2_Bank",
    "TMR_uint16List" => "[Word16]",
    "TMR_GEN2_Password" => "GEN2_Password",
    );

my %listSize = (
    "[Word8]"  => "16",
  # "[Word16]" => "16",
    "[Word32]" => "16",
  # "[Int8]"   => "8",
    "[Region]" => "8",
    "[TagProtocol]" => "8",
    );

# additional comments about parameters (i. e. units)
my %extra = (
    "/reader/commandTimeout" => "milliseconds",
    "/reader/transportTimeout" => "milliseconds",
    "/reader/antenna/settingTimeList" => "microseconds",
    "/reader/gen2/BLF" => "kHz",
    "/reader/gen2/writeReplyTimeout" => "microseconds",
    "/reader/radio/powerMax" => "centi-dBm",
    "/reader/radio/powerMin" => "centi-dBm",
    "/reader/radio/portReadPowerList" => "centi-dBm",
    "/reader/radio/portWritePowerList" => "centi-dBm",
    "/reader/radio/readPower" => "centi-dBm",
    "/reader/radio/writePower" => "centi-dBm",
    "/reader/radio/temperature" => "degrees C",
    "/reader/read/asyncOffTime" => "milliseconds",
    "/reader/read/asyncOnTime" => "milliseconds",
    "/reader/region/hopTable" => "kHz",
    "/reader/region/hopTime" => "milliseconds",
    );

# typedefs for parameters
my %typedefs = (
    "PARAM_GEN2_ACCESSPASSWORD" => "GEN2_Password",
    "PARAM_ANTENNA_PORTLIST" => "[AntennaPort]",
    "PARAM_ANTENNA_CONNECTEDPORTLIST" => "[AntennaPort]",
    "PARAM_ANTENNA_PORTSWITCHGPOS" => "[PinNumber]",
    "PARAM_GPIO_INPUTLIST" => "[PinNumber]",
    "PARAM_GPIO_OUTPUTLIST" => "[PinNumber]",
    "PARAM_TAGOP_ANTENNA" => "AntennaPort",
    "PARAM_TRIGGER_READ_GPI" => "[PinNumber]",
    );

sub readStatus {
    open F, "$apiDir/tmr_status.h" or die;
    my $comment = "";
    while (<F>) {
        if (/^#define TMR_(ERROR_TYPE_[A-Z]+)/) {
            push @errorTypes, $1;
        } elsif (m%^/\*\*(.*?)\s+\*/%) {
            $comment = $1;
        } elsif (/^#define TMR_(ERROR_[A-Z0-9_]+)\s+/) {
            push @errorCodes, $1;
            $errorCodes{$1} = escapeHaddock($comment);
            $comment = "";
        } else {
            $comment = "";
        }
    }
    close F;
}

sub readStrerror {
    open F, "$apiDir/tmr_strerror.c" or die;
    my $status = "";
    while (<F>) {
        if (/^\s+case\s+TMR_(ERROR_\w+):/) {
            $status = $1;
        } elsif (/^\s+return\s+"([^"]+)";/) {
            my $msg = $1;
            if (exists $errorCodes{$status} and $errorCodes{$status} eq "") {
                $errorCodes{$status} = escapeHaddock($msg);
            }
            $status = "";
        } else {
            $status = "";
        }
    }
    close F;
}

sub parseParamComment {
    my ($c, $param) = @_;
    if ($c =~ /^\"([^\"]+)\",\s+(\w+)/) {
        my ($quoted, $type) = ($1, $2);
        my $haskellType;
        if (exists $toHaskellType{$type}) {
            $haskellType = $toHaskellType{$type};
        } else {
            $haskellType = $nyi;
        }
        my $equoted = escapeHaddock($quoted);
        my $linkedType = $haskellType;
        $linkedType =~ s/(\w+)/'$1'/ if ($linkedType ne $nyi);
        my $xtra = "";
        if ($linkedType ne $nyi) {
            if (exists $typedefs{$param}) {
                my $typedef = $typedefs{$param};
                $typedef =~ s/(\w+)/'$1'/;
                $linkedType .= ", or typedef $typedef";
            }
            if (exists $extra{$quoted}) {
                $xtra = escapeHaddock($extra{$quoted});
            }
            if (exists $paramReadOnly{$param}) {
                $xtra .= ", " if ($xtra ne "");
                $xtra .= "read-only";
            }
            $xtra = " ($xtra)" if ($xtra ne "");
        }
        return ($haskellType, "\@$equoted\@ $linkedType$xtra", $quoted);
    } else {
        my $ec = escapeHaddock($c);
        $ec =~ s/(return value from) TMR_paramID\(\)/$1 'System.Hardware.MercuryApi.paramID'/;
        return ("", $ec, "");
    }
}

sub readParams {
    open F, "$apiDir/tmr_params.h" or die;
    my $comment = "";
    while (<F>) {
        if (m%^\s*/\*\*\s*(.*?)\s*\*+/%) {
            $comment = $1;
        } elsif (/^\s*TMR_(PARAM_[A-Z0-9_]+)/) {
            unless ($1 eq "PARAM_MIN" or
                    $1 eq "PARAM_END" or
                    $1 eq "PARAM_MAX") {
                my $p = $1;
                push @params, $p;
                my ($t, $c, $path) = parseParamComment ($comment, $p);
                $params{$p} = $c;
                $paramType{$p} = $t;
                $paramPath{$p} = $path;
            }
            $comment = "";
        } else {
            $comment = "";
        }
    }
    close F;
}

sub readGlue {
    open F, "$glueDir/glue.h" or die;
    my $comment = "";
    while (<F>) {
        if (/^#define (ERROR_TYPE_[A-Z]+)/) {
            push @glueTypes, $1;
            $glueTypes{$1} = escapeHaddock($comment);
            $comment = "";
        } elsif (m%^/\*\*\s*(.*?)\s*\*/%) {
            $comment = $1;
        } elsif (/^#define (ERROR_[A-Z0-9_]+)\s+/) {
            push @glueCodes, $1;
            $glueCodes{$1} = escapeHaddock($comment);
            $comment = "";
        } else {
            $comment = "";
        }
    }
    close F;
}

sub readRegion {
    open F, "$apiDir/tmr_region.h" or die;
    while (<F>) {
        if (m%^\s*/\*\*\s*(.*?)\s*\*+/\s*TMR_(REGION_\w+)%) {
            push @regions, $2;
            $regions{$2} = escapeHaddock($1);
            $regionsUnescaped{$2} = $1;
        }
    }
    close F;
}

sub readTagProtocol {
    open F, "$apiDir/tmr_tag_protocol.h" or die;
    while (<F>) {
        if (/^\s+TMR_(TAG_PROTOCOL_\w+)/) {
            push @tagProtocols, $1;
        }
    }
    close F;
}

sub readStructs {
    my ($file, $structs) = @_;

    my $structName = undef;
    my $comment = "";

    open F, $file or die;
    while (<F>) {
        if (/^typedef struct (\w+)/) {
            $structName = $1;
            $comment = "";
        } elsif (/^\}/) {
            $structName = undef;
        } elsif (m%^\s+/\*+\s*(.*?)\s*\*+/%) {
            $comment = $1;
        } elsif (/^\s+(\w+)([\*\s]+)(\w+)([\w\[\]]*?);/ and defined $structName) {
            my ($fieldName, $type, $dim, $ptr) = ($3, $1, $4, $2);
            push @{$structs->{$structName}{'fields'}}, $fieldName;
            $structs->{$structName}{'type'}{$fieldName} = $type;
            $structs->{$structName}{'comment'}{$fieldName} = escapeHaddock($comment);
            $dim =~ s/[\[\]]//g;
            $structs->{$structName}{'dim'}{$fieldName} = $dim if ($dim ne "");
            $ptr =~ s/\s//g;
            $structs->{$structName}{'ptr'}{$fieldName} = $ptr if ($ptr ne "");
            $comment = "";
        }
    }
    close F;
}

sub readTagData {
    my $file = "$apiDir/tmr_tag_data.h";
    open F, $file or die;
    while (<F>) {
        if (/^\s+TMR_TRD_(METADATA_FLAG_\w+)\s*=/) {
            my $flag = $1;
            if ($flag ne "METADATA_FLAG_NONE" and
                $flag ne "METADATA_FLAG_ALL") {
                push @metadataFlags, $flag;
            }
        }
    }
    close F;

    readStructs ($file, \%tagDataStructs);
}

sub readGen2 {
    my $file = "$apiDir/tmr_gen2.h";
    open F, $file or die;
    my $comment = "";
    while (<F>) {
        if (/^\s+TMR_(GEN2_BANK_[A-Z]+)\s/) {
            my $bank = $1;
            push @banks, $bank;
            $banks{$bank} = escapeHaddock($comment);
            $comment = "";
        } elsif (/^\s+TMR_(GEN2_LOCK_BITS_\w+)\s/) {
            my $lockBit = $1;
            push @lockBits, $lockBit;
            $lockBits{$lockBit} = escapeHaddock($comment);
            $comment = "";
        } elsif (m%^\s+/\*\*\s*(.*?)\s*\*/%) {
            $comment = $1;
        } else {
            $comment = "";
        }
    }
    close F;
    readStructs ($file, \%gen2Structs);
}

sub readGpio {
    readStructs ("$apiDir/tmr_gpio.h", \%gpioStructs);
}

sub readTagop {
    readStructs ("$apiDir/tmr_tagop.h", \%tagopStructs);
}

sub readSerialReader {
    open F, "$apiDir/serial_reader.c" or die;
    my @statuses;
    while (<F>) {
        if (/^\s+case\s+TMR_(PARAM_\w+):/) {
            push @statuses, $1;
        } elsif (/^\s+ret = TMR_ERROR_READONLY;/) {
            foreach my $status (@statuses) {
                $paramReadOnly{$status} = 1;
            }
        } elsif (not /^[\s\{]*$/) {
            @statuses = ();
        }
    }
    close F;
}

sub emit {
    my ($s) = @_;
    push @lines, $s;
}

sub emitHeader {
    open F, "$utilDir/header.hsc" or die;

    while (<F>) {
        chomp;
        if (/^-- This file is inserted/) {
            emit "-- Automatically generated by util/generate-tmr-hsc.pl";
        } else {
            emit $_;
        }
    }

    close F;

    emit "";
    emit "-- end of code inserted from util/header.hsc";
    emit "";
}

sub dumpOutput {
    my ($file) = @_;

    open F, ">$file" or die;
    for my $line (@lines) {
        print F "$line\n";
    }
    close F;

    @lines = ();
}

sub escapeHaddock {
    my ($x) = @_;

    $x =~ s%(['"/\\`@<])%\\$1%g;
    return $x;
}

sub makeComment {
    my ($x) = @_;

    if ($x eq "") {
        return "";
    } else {
        return " -- ^ $x";
    }
}

sub emitEnum1 {
    my ($constructors, $comments, $sep) = @_;
    for my $con (@$constructors) {
        my $comment = "";
        if (exists $comments->{$con} and $comments->{$con} ne "") {
            $comment = makeComment ($comments->{$con});
        }
        emit "  $sep $con$comment";
        $sep = "|";
    }
}

sub emitEnum {
    my ($constructors, $comments) = @_;
    emitEnum1 ($constructors, $comments, " ");
}

sub emitEnumCont {
    my ($constructors, $comments) = @_;
    emitEnum1 ($constructors, $comments, "|");
}

sub emitTo {
    my ($func, $prefix, $constructors) = @_;
    for my $con (@$constructors) {
        emit "$func #{const $prefix$con} = $con";
    }
}

sub emitFrom {
    my ($func, $prefix, $constructors) = @_;
    for my $con (@$constructors) {
        emit "$func $con = #{const $prefix$con}";
    }
}

sub emitStatus {
    emit "-- | Indicates a general category of error.";
    emit "data StatusType =";
    emitEnum (\@errorTypes, {});
    emitEnumCont (\@glueTypes, \%glueTypes);
    emit "  | ERROR_TYPE_UNKNOWN -- ^ Not a recognized status type";
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "toStatusType :: Word32 -> StatusType";
    emitTo ("toStatusType", "TMR_", \@errorTypes);
    emitTo ("toStatusType", "",     \@glueTypes);
    emit "toStatusType _ = ERROR_TYPE_UNKNOWN";
    emit "";

    emit "-- | A specific error encountered by the C API or the Haskell binding.";
    emit "data Status =";
    emitEnum (\@errorCodes, \%errorCodes);
    emitEnumCont (\@glueCodes, \%glueCodes);
    emitEnumCont ([sort keys %haskellCodes], \%haskellCodes);
    emit "  | ERROR_UNKNOWN Word32 -- ^ C API returned an unrecognized status code";
    emit "  deriving (Eq, Ord, Show, Read)";
    emit "";

    emit "toStatus :: Word32 -> Status";
    emitTo ("toStatus", "TMR_", \@errorCodes);
    emitTo ("toStatus", "",     \@glueCodes);
    emit "toStatus x = ERROR_UNKNOWN x";
    emit "";
}

sub emitParams {
    emit "type RawParam = #{type TMR_Param}";
    emit "";

    emit "-- | Reader parameters which you can get and set.  The names";
    emit "-- are the same as the names of the enum in the C API.";
    emit "-- (Unfortunately, these do not correspond to the";
    emit "-- \\\"path\\\"-style names in any systematic way.)";
    emit "-- Each parameter is listed with its \\\"path\\\", and the";
    emit "-- Haskell type which is used to store it.  Some parameters";
    emit "-- are also listed with the physical units the parameter";
    emit "-- is in.  Not all parameters are implemented in the Haskell";
    emit "-- binding.  Please file a Github issue if there is a parameter";
    emit "-- you need which is not implemented.";
    emit "data Param =";
    emitEnum ([sort compareParam @params], \%params);
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "instance Hashable Param where";
    emit "  hash = fromEnum";
    emit "  salt `hashWithSalt` p = salt `hashWithSalt` fromEnum p";
    emit "";

    emit "toParam :: RawParam -> Param";
    emitTo ("toParam", "TMR_", \@params);
    emit "toParam _ = PARAM_NONE";
    emit "";

    emit "fromParam :: Param -> RawParam";
    emitFrom ("fromParam", "TMR_", \@params);
    emit "";

    emit "paramMax :: RawParam";
    emit "paramMax = #{const TMR_PARAM_MAX}";
    emit "";

    emit "-- | For parameters which are expressed in physical units,";
    emit "-- returns a string describing the units.  Returns 'Nothing'";
    emit "-- for parameters which are not expressed in physical units.";
    emit "-- This can be useful for displaying in a user interface,";
    emit "-- for example.";
    emit "paramUnits :: Param -> Maybe Text";
    foreach my $param (@params) {
        my $path = $paramPath{$param};
        if (exists $extra{$path}) {
            my $xtra = $extra{$path};
            emit "paramUnits $param = Just \"$xtra\"";
        }
    }
    emit "paramUnits _ = Nothing";
    emit "";
}

sub emitStruct {
    my ($name, $prefix, $fields, $types) = @_;

    emit "data $name =";
    emit "  $name";
    my $sep = "{";
    foreach my $field (@$fields) {
        my $type = $types->{$field};
        emit "  $sep ${prefix}_$field :: !($type)";
        $sep = ",";
    }
    emit "  }";
    emit "";

    emit "instance Storable $name where";
    emit "  sizeOf _ = #{size $name}";
    emit "  alignment _ = 8"; # because "#alignment" doesn't work for me
    emit "  peek p = $name";
    $sep = '<$>';
    foreach my $field (@$fields) {
        emit "           $sep #{peek $name, $field} p";
        $sep = '<*>';
    }
    emit "  poke p x = do";
    foreach my $field (@$fields) {
        emit "    #{poke $name, $field} p (${prefix}_$field x)";
    }
    emit "";
}

sub emitListStruct {
    my ($size) = @_;

    my $word = "Word$size";
    my @fields = ("list", "max", "len");
    my %fields = ("list" => "Ptr ()", "max" => $word, "len" => $word );
    emitStruct ("List$size", "l$size", \@fields, \%fields);
}

sub emitListFuncs {
    my ($size) = @_;

    emit "getList$size :: Storable a => (Ptr () -> IO ()) -> IO [a]";
    emit "getList$size f = do";
    emit "  let maxLen = maxBound :: Word${size}";
    emit '  allocaArray (fromIntegral maxLen) $ \storage -> do';
    emit "    let lst = List${size}";
    emit "              { l${size}_list = castPtr storage";
    emit "              , l${size}_max = maxLen";
    emit "              , l${size}_len = 0";
    emit '              }';
    emit '    with lst $ \p -> do';
    emit '      f (castPtr p)';
    emit "      lst' <- peek p";
    emit "      peekArray (fromIntegral (l${size}_len lst')) storage";
    emit '';
    emit "setList$size :: Storable a => Text -> [a] -> (Ptr () -> IO ()) -> IO ()";
    emit "setList$size t x f = do";
    emit '  withArrayLen x $ \len storage -> do';
    emit "    len' <- castLen t len";
    emit "    let lst = List${size}";
    emit "              { l${size}_list = castPtr storage";
    emit "              , l${size}_max = len'";
    emit "              , l${size}_len = len'";
    emit '              }';
    emit '    with lst $ \p -> f (castPtr p)';
    emit "";

    emit "pokeList$size :: Storable a => (Ptr List$size, Word$size, Ptr a, Text) -> [a] -> IO ()";
    emit "pokeList$size (lp, maxLen, storage, name) ws = do";
    emit "  len <- castLen' maxLen name (length ws)";
    emit "  poke lp \$ List$size";
    emit "    { l${size}_list = castPtr storage";
    emit "    , l${size}_max = maxLen";
    emit "    , l${size}_len = len";
    emit "    }";
    emit "  pokeArray storage ws";
    emit "";
    emit "peekList$size :: Storable a => (Ptr List$size, Word$size, Ptr a, Text) -> IO [a]";
    emit "peekList$size (lp, _, _, _) = do";
    emit "  lst <- peek lp";
    emit "  peekArray (fromIntegral \$ l${size}_len lst) (castPtr \$ l${size}_list lst)";
    emit "";
}

sub convertStruct {
    my ($cStruct, $fieldOrder, $fields) = @_;

    foreach my $field (@{$cStruct->{'fields'}}) {
        push @$fieldOrder, $field;

        my $cType = $cStruct->{'type'}{$field};
        my $comment = $cStruct->{'comment'}{$field};

        my $hsType = "undefined";
        if (exists $toHaskellType2{$cType}) {
            $hsType = $toHaskellType2{$cType};
       }

        my %info = (
            "c" => [$field],
            "type" => $hsType,
            "comment" => $comment,
            "marshall" => ["peek", "poke"]
            );

        if (exists $cStruct->{'dim'}{$field}) {
            $info{"dim"} = $cStruct->{'dim'}{$field};
        }

        $fields->{$field} = \%info;
    }
}

sub makePfield {
    my ($f) = @_;
    croak "undefined argument to makePfield" if (not defined $f);
    $f =~ s/\W/_/g;
    my $u = ucfirst ($f);
    return "p$u";
}

sub emitData {
    my ($prefix, $fields, $info, $bang, $indent) = @_;

    my $sep = "{";
    foreach my $field (@$fields) {
        if (exists $info->{$field}) {
            my $fieldType = $info->{$field}{"type"};
            my $comment = $info->{$field}{"comment"};
            my $ufield = ucfirst ($field);
            croak "comment undefined for $field" if (not defined $comment);
            $comment = makeComment ($comment);
            emit "$indent$sep $prefix$ufield :: $bang($fieldType)$comment";
            $sep = ",";
        }
    }
}

sub emitPeek {
    my ($constructor, $cType, $fields, $info, $indent) = @_;

    emit "$indent$constructor";
    my $sep = '<$>';
    foreach my $field (@$fields) {
        if (exists $info->{$field}) {
            my $c = $info->{$field}{"c"};
            my $marshall = $info->{$field}{"marshall"}[0];
            my $filter = "";
            if (exists $info->{$field}{"filter"}) {
                $filter = $info->{$field}{"filter"}[0];
                $filter .= ' <$> ';
            }
            my @ptrs = map { "(#{ptr $cType, $_} p)" } @$c;
            emit ("$indent  $sep ($filter$marshall " . join (" ", @ptrs) . ")");
            $sep = '<*>';
        }
    }
}

sub emitPoke {
    my ($prefix, $cType, $fields, $info, $indent) = @_;

    foreach my $field (@$fields) {
        if (exists $info->{$field}) {
            my $c = $info->{$field}{"c"};
            my $marshall = $info->{$field}{"marshall"}[1];
            my $filter = "";
            if (exists $info->{$field}{"filter"}) {
                $filter = $info->{$field}{"filter"}[1];
                $filter .= ' $ ';
            }
            my $hField = $prefix . ucfirst ($field);
            my @ptrs = map ("(#{ptr $cType, $_} p)", @$c);
            emit ("$indent$marshall " . join (" ", @ptrs) . " ($filter$hField x)");
        }
    }
}

sub emitStruct2 {
    my ($hType, $prefix, $cType, $fields, $info, $poke) = @_;

    my $nFields = 0;
    foreach my $field (@$fields) {
        $nFields++ if (exists $info->{$field});
    }

    my $data = "data";
    my $bang = "!";
    if ($nFields == 1) {
        $data = "newtype";
        $bang = "";
    }

    emit "$data $hType =";
    emit "  $hType";
    emitData ($prefix, $fields, $info, $bang, "  ");
    emit "  } deriving (Eq, Ord, Show, Read)";
    emit "";

    emit "instance Storable $hType where";
    emit "  sizeOf _ = #{size $cType}";
    emit "  alignment _ = 8"; # because "#alignment" doesn't work for me
    emit "";
    emit "  peek p =";
    emitPeek ($hType, $cType, $fields, $info, "    ");
    emit "";

    if ($poke) {
        emit "  poke p x = do";
        emitPoke ($prefix, $cType, $fields, $info, "    ");
    } else {
        emit "  poke p x = error \"poke not implemented for $hType\"";
    }

    emit "";
}

# discriminator{'field'} = string (C field name)
# discriminator{'type'}  = string (C type name)
# constInfo{constructor}{'fields'}        = array
# constInfo{constructor}{'discriminator'} = string (C constant value)
# constInfo{constructor}{'info'}{field}{'c'} = array of C field names
# constInfo{constructor}{'info'}{field}{'type'} = string (Haskell type)
# constInfo{constructor}{'info'}{field}{'comment'} = string
# constInfo{constructor}{'info'}{field}{'marshall'} = array of length 2
# constInfo{constructor}{'info'}{field}{'filter'} = array of length 2
sub emitUnion {
    my ($hType, $prefix, $cType, $discriminator, $constructors, $constInfo) = @_;
    my $dField = $discriminator->{'field'};
    my $dType = $discriminator->{'type'};

    emit "data $hType =";
    my $sep = " ";
    foreach my $const (@$constructors) {
        my $cInfo  = $constInfo->{$const};
        my $fields = $cInfo->{'fields'};
        my $info   = $cInfo->{'info'};
        emit "  $sep $const";
        emitData ($prefix, $fields, $info, "!", "    ");
        emit "    }";
        $sep = "|";
    }
    emit "  deriving (Eq, Ord, Show, Read)";
    emit "";

    emit "instance Storable $hType where";
    emit "  sizeOf _ = #{size $cType}";
    emit "  alignment _ = 8"; # because "#alignment" doesn't work for me
    emit "";
    emit "  peek p = do";
    emit "    x <- #{peek $cType, $dField} p :: IO #{type $dType}";
    emit "    case x of";
    foreach my $const (@$constructors) {
        my $cInfo  = $constInfo->{$const};
        my $fields = $cInfo->{'fields'};
        my $info   = $cInfo->{'info'};
        my $dValue = $cInfo->{'discriminator'};
        emit "      #{const $dValue} -> do";
        emitPeek ($const, $cType, $fields, $info, "        ");
    }
    emit "";

    foreach my $const (@$constructors) {
        my $cInfo  = $constInfo->{$const};
        my $fields = $cInfo->{'fields'};
        my $info   = $cInfo->{'info'};
        my $dValue = $cInfo->{'discriminator'};
        emit "  poke p x\@($const {}) = do";
        emit "    #{poke $cType, $dField} p (#{const $dValue} :: #{type $dType})";
        emitPoke ($prefix, $cType, $fields, $info, "    ");
        emit "";
    }
}

sub byteStringArrayField {
    my ($fields, $arrayField, $lengthField) = @_;

    my $info = $fields->{$arrayField};
    my $maxLen = $info->{"dim"};
    $info->{"c"} = [$arrayField, $lengthField];
    $info->{"type"} = "ByteString";
    $info->{"marshall"} = ["peekArrayAsByteString",
                           "pokeArrayAsByteString \"$arrayField\" #{const $maxLen}"];

    delete $fields->{$lengthField};
}

sub byteStringListField {
    my ($fields, $field) = @_;

    my $info = $fields->{$field};
    $info->{"type"} = "ByteString";
    $info->{"marshall"} = ["peekListAsByteString", "pokeListAsByteString"];
}

sub listArrayField {
    my ($fields, $arrayField, $lengthField, $type) = @_;

    my $info = $fields->{$arrayField};
    $info->{"c"} = [$arrayField, $lengthField];
    $info->{"type"} = $type;
    $info->{"marshall"} = ["peekArrayAsList", "pokeArrayAsList"];

    delete $fields->{$lengthField};
}

sub listListField {
    my ($fields, $listField, $storageField, $maxLen) = @_;

    my $info = $fields->{$listField};
    push @{$info->{"c"}}, $storageField;
    $info->{"marshall"} = ["peekListAsList",
                           "pokeListAsList \"$listField\" #{const $maxLen}"];
}

sub maybeField {
    my ($fields, $justField, $conditionField, $condition) = @_;

    my $info = $fields->{$justField};
    my $oldType = $info->{"type"};
    my $oldPeek = $info->{"marshall"}[0];
    my $oldPoke = $info->{"marshall"}[1];
    $info->{"c"} = [$justField, $conditionField];
    $info->{"type"} = "Maybe ($oldType)";
    $info->{"marshall"} = ["peekMaybe ($oldPeek) ($condition)",
                           "pokeMaybe ($oldPoke) ($condition)"];
}

sub wrapField {
    my ($fields, $field, $peekWrap, $pokeWrap) = @_;

    my $info = $fields->{$field};
    $info->{"filter"} = [$peekWrap, $pokeWrap]
}

sub split64Field {
    my ($fieldOrder, $fields, $field, $comment) = @_;

    my ($lo, $hi) = ("${field}Low", "${field}High");

    my %info;
    $info{"c"} = [$lo, $hi];
    $info{"type"} = "MillisecondsSinceEpoch";
    $info{"comment"} = $comment;
    $info{"marshall"} = ["peekSplit64", "pokeSplit64"];

    delete $fields->{$lo};
    delete $fields->{$hi};
    $fields->{$field} = \%info;

    my @newOrder;
    foreach my $f (@$fieldOrder) {
        if ($f eq $lo) {
            push @newOrder, $field;
        } elsif ($f ne $hi) {
            push @newOrder, $f;
        }
    }
    @$fieldOrder = @newOrder;
}

sub renameField {
    my ($fieldOrder, $fields, $oldField, $newField) = @_;

    $fields->{$newField} = $fields->{$oldField};
    delete $fields->{$oldField};

    my @newOrder;
    foreach my $f (@$fieldOrder) {
        if ($f eq $oldField) {
            push @newOrder, $newField;
        } else {
            push @newOrder, $f;
        }
    }
    @$fieldOrder = @newOrder;
}

sub emitGen2 {
    emit "-- | Gen2-specific per-tag data";

    my $cName = "TMR_GEN2_TagData";
    my $cStruct = $gen2Structs{$cName};

    my @fieldOrder;
    my %fields;

    convertStruct ($cStruct, \@fieldOrder, \%fields);

    byteStringArrayField (\%fields, "pc", "pcByteCount");

    emitStruct2 ("GEN2_TagData", "g2", $cName, \@fieldOrder, \%fields, 1);
}

sub emitTagData {
    emit "-- | A record to represent RFID tags.";

    my $cName = "TMR_TagData";
    my $cStruct = $tagDataStructs{$cName};

    my @fieldOrder;
    my %fields;

    convertStruct ($cStruct, \@fieldOrder, \%fields);

    byteStringArrayField (\%fields, "epc", "epcByteCount");
    maybeField (\%fields, "gen2", "protocol",
                "== (#{const TMR_TAG_PROTOCOL_GEN2} :: RawTagProtocol)");
    $fields{"gen2"}{"c"}[0] = "u.gen2";
    $fields{"gen2"}{"marshall"}[1] = "pokeGen2TagData";
    wrapField (\%fields, "protocol", "toTagProtocol", "fromTagProtocol");

    emitStruct2 ("TagData", "td", $cName, \@fieldOrder, \%fields, 1);
}

sub emitGpio {
    emit "-- | The identity and state of a single GPIO pin.";

    my $cName = "TMR_GpioPin";
    my $cStruct = $gpioStructs{$cName};

    my @fieldOrder;
    my %fields;

    convertStruct ($cStruct, \@fieldOrder, \%fields);

    wrapField (\%fields, "high", "toBool'", "fromBool'");
    wrapField (\%fields, "output", "toBool'", "fromBool'");

    emitStruct2 ("GpioPin", "gp", $cName, \@fieldOrder, \%fields, 1);
}

sub deleteUnderscoreFields {
    my ($fields) = @_;

    foreach my $field (keys %$fields) {
        delete $fields->{$field} if ($field =~ /^_/);
    }
}

sub emitTagReadData {
    emit "-- | A record to represent a read of an RFID tag.";
    emit "-- Provides access to the metadata of the read event,";
    emit "-- such as the time of the read, the antenna that read the tag,";
    emit "-- and the number of times the tag was seen by the air protocol.";

    my $cName = "TMR_TagReadData";
    my $cStruct = $tagDataStructs{$cName};

    my @fieldOrder;
    my %fields;

    convertStruct ($cStruct, \@fieldOrder, \%fields);

    # delete private fields
    delete $fields{"isAsyncRead"};
    delete $fields{"dspMicros"};
    delete $fields{"reader"};
    deleteUnderscoreFields (\%fields);

    # fields that need special handling:
    # use AntennaPort typedef instead of Word8
    $fields{"antenna"}{"type"} = "AntennaPort";

    # metadataFlags as list of flags
    wrapField (\%fields, "metadataFlags", "unpackFlags16", "packFlags16");
    $fields{"metadataFlags"}{"type"} = "[MetadataFlag]";

    # gpio/gpioCount as array
    listArrayField (\%fields, "gpio", "gpioCount", "[GpioPin]");

    # timestampLow/timestampHigh merged into one
    split64Field (\@fieldOrder, \%fields, "timestamp",
                  escapeHaddock("Absolute time of the read, in milliseconds since 1/1/1970 UTC"));

    # uint8Lists as bytestrings
    foreach my $f (qw(data epcMemData tidMemData userMemData reservedMemData)) {
        byteStringListField (\%fields, $f);
        if ($fields{$f}{"comment"} =~ /^Read (\w+) bank data bytes/) {
            $fields{$f}{"comment"} .=
                "  (Only if 'GEN2_BANK_$1' is present in 'opExtraBanks')";
        }
    }

    emitStruct2 ("TagReadData", "tr", $cName, \@fieldOrder, \%fields, 0);
}

sub emitTagOp {
    my @ops = (qw(writeTag writeData readData lock kill));

    my $hType = "TagOp";
    my $prefix = "op";
    my $cType = "TagOpEtc";
    my %discriminator = ("field" => "tagop.type", "type" => "TMR_TagOpType");
    my @constructors;
    my %constInfo;

    foreach my $op (@ops) {
        my $const = "TagOp_GEN2_" . ucfirst($op);
        my $struct = "TMR_$const";
        my $disc = uc($struct);
        my $s = $tagopStructs{$struct};
        my @fields;
        my %info;

        convertStruct ($s, \@fields, \%info);
        foreach my $field (@fields) {
            my $i = $info{$field};
            my @newC;
            foreach my $c (@{$i->{"c"}}) {
                push @newC, "tagop.u.gen2.u.$op.$c";
            }
            $i->{"c"} = \@newC;
        }

        push @constructors, $const;
        $constInfo{$const}{'fields'} = \@fields;
        $constInfo{$const}{'discriminator'} = $disc;
        $constInfo{$const}{'info'} = \%info;
    }

    # Ugh, now all the special cases
    my $writeTag = $constInfo{'TagOp_GEN2_WriteTag'};
    renameField ($writeTag->{'fields'}, $writeTag->{'info'}, "epcptr", "epc");
    my $epc = $writeTag->{'info'}{"epc"};
    push @{$epc->{'c'}}, "epc";
    $epc->{'marshall'} = ["peekPtr", "pokePtr"];

    foreach my $const ("TagOp_GEN2_WriteData", "TagOp_GEN2_ReadData") {
        wrapField ($constInfo{$const}{"info"}, "bank",
                   "(toBank . (.&. 3))", "fromBank");
    }

    my $writeData = $constInfo{"TagOp_GEN2_WriteData"};
    listListField ($writeData->{'info'}, "data",
                   "data16", "GLUE_MAX_DATA16");

    my $readData       = $constInfo{"TagOp_GEN2_ReadData"};
    my $readDataFields = $readData->{'fields'};
    my $readDataInfo   = $readData->{'info'};
    my @newFields;
    foreach my $field (@$readDataFields) {
        push @newFields, $field;
        if ($field eq 'bank') {
            my %newInfo = %{$readDataInfo->{$field}};
            push @newFields, "extraBanks";
            $newInfo{'type'} = "[GEN2_Bank]";
            $newInfo{'comment'} =
                "Additional Gen2 memory banks to read from  " .
                "(seems buggy, though; I\\'ve had strange results with it)";
            $newInfo{'marshall'} = ["peek", "pokeOr"];
            $readDataInfo->{"extraBanks"} = \%newInfo;
        }
    }
    @$readDataFields = @newFields;
    wrapField ($readDataInfo, "extraBanks", "unpackExtraBanks", "packExtraBanks");

    my $lockInfo = $constInfo{"TagOp_GEN2_Lock"}{"info"};
    foreach my $field ("mask", "action") {
        wrapField ($lockInfo, $field, "unpackLockBits16", "packLockBits16");
        $lockInfo->{$field}{'type'} = "[GEN2_LockBits]";
    }

    emit "-- | An operation that can be performed on a tag.  Can be used";
    emit '-- as an argument to @executeTagOp@, or can be embedded into';
    emit "-- a read plan.";
    emitUnion ($hType, $prefix, $cType,
               \%discriminator, \@constructors, \%constInfo);
}

sub emitStructs {
    emitListStruct ("16");
    emitListFuncs ("16");
    emitListStruct ("8");
    emitListFuncs ("8");

    emitGen2();
    emitTagData();
    emitGpio();
    emitTagReadData();
    emitTagOp();
}

sub paramTypeName {
    my ($t) = @_;

    if ($t =~ /^\[(\w+)\]$/) {
        return "ParamType${1}List";
    } else {
        return "ParamType$t";
    }
}

sub emitParamTypes {
    my %ptn;
    foreach my $paramType (values %toHaskellType) {
        $ptn{$paramType} = paramTypeName ($paramType);
    }

    emit "-- | The Haskell data type expected for a particular parameter.";
    emit "data ParamType =";
    emitEnum ([sort values %ptn], {});
    emit "  | ParamTypeUnimplemented";
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "-- | Indicates the type expected for a given parameter.";
    emit "paramType :: Param -> ParamType";
    foreach my $param (@params) {
        my $type = $paramType{$param};
        if (exists $ptn{$type}) {
            my $name = $ptn{$type};
            emit "paramType $param = $name";
        }
    }
    emit "paramType _ = ParamTypeUnimplemented";
    emit "";

    emit "-- | A textual representation of the Haskell type corresponding";
    emit "-- to a particular 'ParamType'.";
    emit "displayParamType :: ParamType -> Text";
    foreach my $paramType (sort keys %ptn) {
        my $name = $ptn{$paramType};
        emit "displayParamType $name = \"$paramType\"";
    }
    emit "displayParamType _ = \"$nyi\"";

    foreach my $paramType (sort keys %ptn) {
        my $name = $ptn{$paramType};
        emit "";
        emit "instance ParamValue $paramType where";
        emit "  pType _ = $name";
        if ($paramType =~ /^Int/ or $paramType =~ /^Word/) {
            emit '  pGet f = alloca $ \p -> f (castPtr p) >> peek p';
            emit '  pSet x f = alloca $ \p -> poke p x >> f (castPtr p)';
        } elsif ($paramType eq "Bool") {
            emit '  pGet f = alloca $ \p -> f (castPtr (p :: Ptr CBool)) >> toBool <$> peek p';
            emit '  pSet x f = alloca $ \p -> poke p (fromBool x :: CBool) >> f (castPtr p)';
        } elsif ($paramType eq "Region") {
            emit '  pGet f = alloca $ \p -> f (castPtr p) >> toRegion <$> peek p';
            emit '  pSet x f = alloca $ \p -> poke p (fromRegion x) >> f (castPtr p)';
        } elsif ($paramType eq "TagProtocol") {
            emit '  pGet f = alloca $ \p -> f (castPtr p) >> toTagProtocol <$> peek p';
            emit '  pSet x f = alloca $ \p -> poke p (fromTagProtocol x) >> f (castPtr p)';
        } elsif ($paramType eq "ReadPlan") {
            emit '  pGet f = alloca $ \p -> f (castPtr p) >> peek p';
            # Unlike all the other cases, in this case we transfer ownership
            # to the C code, which will free it later.
            emit '  pSet x f = bracketOnError (new x) free (f . castPtr)';
        } elsif ($paramType eq "[MetadataFlag]") {
            emit '  pGet f = alloca $ \p -> f (castPtr p) >> unpackFlags <$> peek p';
            emit '  pSet x f = alloca $ \p -> poke p (packFlags x) >> f (castPtr p)';
        } elsif ($paramType eq "Text") {
            emit '';
            emit '  pGet f = do';
            emit '    let maxLen = maxBound :: Word16';
            emit '    allocaBytes (fromIntegral maxLen) $ \storage -> do';
            emit '      let lst = List16';
            emit '                { l16_list = castPtr storage';
            emit '                , l16_max = maxLen';
            emit '                , l16_len = 0 -- unused for TMR_String';
            emit '                }';
            emit '      with lst $ \p -> do';
            emit '        f (castPtr p)';
            emit '        textFromCString storage';
            emit '';
            emit '  pSet x f = do';
            emit '    let bs = textToBS x';
            emit '    B.useAsCString bs $ \cs -> do';
            emit "      len' <- castLen \"Text\" (1 + B.length bs)";
            emit '      let lst = List16';
            emit '                { l16_list = castPtr cs';
            emit "                , l16_max = len'";
            emit '                , l16_len = 0 -- unused for TMR_String';
            emit '                }';
            emit '      with lst $ \p -> f (castPtr p)';
        } elsif ($paramType =~ /^\[Int/ or $paramType =~ /^\[Word/) {
            my $size = $listSize{$paramType};
            emit "  pGet = getList$size";
            emit "  pSet = setList$size \"$paramType\"";
        } elsif ($paramType eq "[Region]") {
            my $size = $listSize{$paramType};
            emit "  pGet f = map toRegion <\$> getList$size f";
            emit "  pSet x f = setList$size \"$paramType\" (map fromRegion x) f";
        } elsif ($paramType eq "[TagProtocol]") {
            my $size = $listSize{$paramType};
            emit "  pGet f = map toTagProtocol <\$> getList$size f";
            emit "  pSet x f = setList$size \"$paramType\" (map fromTagProtocol x) f";
        }
    }
    emit "";
}

sub emitRegion {
    emit "type RawRegion = #{type TMR_Region}";
    emit "";

    emit "-- | RFID regulatory regions";
    emit "data Region =";
    emitEnum (\@regions, \%regions);
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "toRegion :: RawRegion -> Region";
    emitTo ("toRegion", "TMR_", \@regions);
    emit "toRegion _ = REGION_NONE";
    emit "";

    emit "fromRegion :: Region -> RawRegion";
    emitFrom ("fromRegion", "TMR_", \@regions);
    emit "";

    emit "-- | A description of the given region, useful for a user interface.";
    emit "displayRegionDescription :: Region -> Text";
    foreach my $region (@regions) {
        my $desc = $regionsUnescaped{$region};
        emit "displayRegionDescription $region = \"$desc\"";
    }
    emit "";
}

sub emitTagProtocol {
    emit "type RawTagProtocol = #{type TMR_TagProtocol}";
    emit "";

    emit "-- | The protocol used by an RFID tag.  Only 'TAG_PROTOCOL_GEN2'";
    emit "-- is supported by the M6e Nano, and therefore the Haskell";
    emit "-- binding currently only supports that protocol.";
    emit "data TagProtocol =";
    emitEnum (\@tagProtocols, {});
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "toTagProtocol :: RawTagProtocol -> TagProtocol";
    emitTo ("toTagProtocol", "TMR_", \@tagProtocols);
    emit "toTagProtocol _ = TAG_PROTOCOL_NONE";
    emit "";

    emit "fromTagProtocol :: TagProtocol -> RawTagProtocol";
    emitFrom ("fromTagProtocol", "TMR_", \@tagProtocols);
    emit "";
}

sub emitMetadataFlags {
    emit "type RawMetadataFlag = #{type TMR_TRD_MetadataFlag}";
    emit "";

    emit "-- | Various metadata parameters which can be requested";
    emit "-- in 'PARAM_METADATAFLAG'.";
    emit "data MetadataFlag =";
    emitEnum (\@metadataFlags, {});
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "fromMetadataFlag :: MetadataFlag -> RawMetadataFlag";
    emitFrom ("fromMetadataFlag", "TMR_TRD_", \@metadataFlags);
    emit "";
}

sub emitBanks {
    emit "type RawBank = #{type TMR_GEN2_Bank}";
    emit "";

    emit "-- | Gen2 memory banks";
    emit "data GEN2_Bank =";
    emitEnum (\@banks, \%banks);
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "fromBank :: GEN2_Bank -> RawBank";
    emitFrom ("fromBank", "TMR_", \@banks);
    emit "";

    emit "toBank :: RawBank -> GEN2_Bank";
    emitTo ("toBank", "TMR_", \@banks);
    emit "toBank x = error \$ \"didn't expect bank to be \" ++ show x";
    emit "";

    emit "fromExtraBank :: GEN2_Bank -> RawBank";
    for my $con (@banks) {
        emit "fromExtraBank $con = #{const TMR_${con}_ENABLED}";
    }
    emit "";
}

sub emitLockBits {
    emit "type RawLockBits = #{type TMR_GEN2_LockBits}";
    emit "";

    emit "-- | Memory lock bits";
    emit "data GEN2_LockBits =";
    emitEnum (\@lockBits, \%lockBits);
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "fromLockBits :: GEN2_LockBits -> RawLockBits";
    emitFrom ("fromLockBits", "TMR_", \@lockBits);
    emit "";
}

sub paramBase {
    my ($x) = @_;
    $x =~ s%/[^/]+$%%;
    return $x;
}

sub paramTriple {
    my ($param) = @_;
    my $path = $paramPath{$param};
    my $base = paramBase($path);

    my $n;
    my $x = $param;
    my $y = $path;
    if ($path ne "") {
        $n = 1;
        $x = $base;
    } elsif ($param eq "PARAM_NONE") {
        $n = 0;
    } else {
        $n = 2;
    }

    return ($n, $x, $y);
}

sub compareParam {
    my ($n1, $x1, $y1) = paramTriple($a);
    my ($n2, $x2, $y2) = paramTriple($b);

    my $nc = $n1 <=> $n2;
    my $xc = $x1 cmp $x2;
    my $yc = $y1 cmp $y2;

    return $nc if ($nc != 0);
    return $xc if ($xc != 0);
    return $yc;
}

sub emitParamHeader {
    emit "-- Automatically generated by util/generate-tmr-hsc.pl";
    emit '{-|';
    emit 'Module      : System.Hardware.MercuryApi.Params';
    emit 'Description : Type-safe parameter access';
    emit 'Copyright   : © Patrick Pelletier, 2017';
    emit 'License     : MIT';
    emit 'Maintainer  : code@funwithsoftware.org';
    emit 'Portability : POSIX';
    emit '';
    emit 'Individual functions to get and set parameters.  These are';
    emit "type-checked at compile time, unlike 'paramGet' and 'paramSet',";
    emit 'which are type-checked at runtime.';
    emit '-}';
    emit "module System.Hardware.MercuryApi.Params";
    emit "  ( -- * Type-safe getters and setters";
    my $sep = " ";
    my $oldbase = "";
    foreach my $param (sort compareParam @params) {
        my $path = $paramPath{$param};
        my $base = paramBase($path);
        my $camel = makeCamel($path);
        my $type = $paramType{$param};
        if ($type ne $nyi and $camel ne "") {
            if ($oldbase ne $base) {
                emit ("    -- ** " . escapeHaddock($base));
                $oldbase = $base;
            }
            emit "  $sep paramGet$camel";
            $sep = ",";
            emit "  $sep paramSet$camel" if (not exists $paramReadOnly{$param});
        }
    }
    emit "    -- * As strings";
    emit "  , paramGetString";
    emit "  , paramSetString";
    emit "  ) where";
    emit "";
    emit "import Control.Applicative";
    emit "import Data.ByteString (ByteString)";
    emit "import Data.Int";
    emit "import Data.Text (Text)";
    emit "import qualified Data.Text as T";
    emit "import Data.Word";
    emit "";
    emit "import System.Hardware.MercuryApi hiding (read)";
    emit "";
}

sub makeCamel {
    my ($camel) = @_;
    $camel =~ s%^/reader%%;
    $camel =~ s%/(.)%uc($1)%eg;
    return $camel;
}

sub emitParamHelpers {
    foreach my $param (@params) {
        my $path = $paramPath{$param};
        my $camel = makeCamel($path);
        my $type = $paramType{$param};
        my $esc = escapeHaddock($path);
        my $com = "";
        if (exists $extra{$path}) {
            $com = " (" . escapeHaddock($extra{$path}) . ")";
        }
        if ($type ne $nyi and $camel ne "") {
            $type = $typedefs{$param} if (exists $typedefs{$param});
            if (not exists $paramReadOnly{$param}) {
                emit "-- | Set parameter '$param' (\@$esc\@)$com";
                emit "paramSet$camel :: Reader -> $type -> IO ()";
                emit "paramSet$camel rdr = paramSet rdr $param";
                emit "";
            }
            emit "-- | Get parameter '$param' (\@$esc\@)$com";
            emit "paramGet$camel :: Reader -> IO $type";
            emit "paramGet$camel rdr = paramGet rdr $param";
            emit "";
        }
    }
}

sub emitParamStringHelpers {
    my %ptn;
    foreach my $paramType (values %toHaskellType) {
        $ptn{$paramType} = paramTypeName ($paramType);
    }

    emit "-- | Version of 'paramSet' which converts its argument from a";
    emit "-- string to the proper type using 'read'.";
    emit "paramSetString :: Reader -> Param -> Text -> IO ()";
    emit "paramSetString rdr param txt = do";
    emit "  let str = T.unpack txt";
    emit "  case paramType param of";
    foreach my $paramType (sort keys %ptn) {
        my $name = $ptn{$paramType};
        emit "    $name -> paramSet rdr param (read str :: $paramType)";
    }
    emit '    _ -> paramSet rdr param (undefined :: Bool) -- force ERROR_UNIMPLEMENTED_PARAM';
    emit "";

    emit "-- | Version of 'paramGet' which converts its result to a";
    emit "-- string using 'show'.";
    emit "paramGetString :: Reader -> Param -> IO Text";
    emit "paramGetString rdr param =";
    emit '  T.pack <$>';
    emit "  case paramType param of";
    foreach my $paramType (sort keys %ptn) {
        my $name = $ptn{$paramType};
        emit "    $name -> show <\$> (paramGet rdr param :: IO $paramType)";
    }
    emit '    _ -> show <$> (paramGet rdr param :: IO Bool) -- force ERROR_UNIMPLEMENTED_PARAM';
    emit "";
}

readStatus();
readStrerror();
readSerialReader(); # must be before readParams()
readParams();
readGlue();
readRegion();
readTagProtocol();
readTagData();
readGen2();
readGpio();
readTagop();

emitHeader();
emitStructs();
emitStatus();
emitRegion();
emitTagProtocol();
emitMetadataFlags();
emitBanks();
emitLockBits();
emitParams();
emitParamTypes();

dumpOutput ($generatedFile);

emitParamHeader();
emitParamHelpers();
emitParamStringHelpers();

dumpOutput ($paramFile);
