#!/usr/bin/perl -w

use strict;
use Carp;
use FindBin;

my $utilDir = $FindBin::Bin;
my $rootDir = "$utilDir/..";
my $apiDir = "$rootDir/cbits/api";
my $glueDir = "$rootDir/cbits/glue";
my $outputFile = "$rootDir/src/System/Hardware/MercuryApi/Generated.hsc";

my @errorTypes = ("SUCCESS_TYPE");
my @errorCodes = ("SUCCESS");
my %errorCodes = ("SUCCESS" => "Success!");
my @glueTypes = ();
my %glueTypes = ();
my @glueCodes = ();
my %glueCodes = ();

my @params = ();
my %params = ();
my %paramType = ();

my @regions = ();
my %regions = ();

my @tagProtocols = ();

my @metadataFlags = ();

my %tagDataStructs = ();
my %gen2Structs = ();
my %gpioStructs = ();

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
    "TMR_TagData" => "TagData"
    );

my %listSize = (
    "[Word8]"  => "16",
  # "[Word16]" => "16",
    "[Word32]" => "16",
  # "[Int8]"   => "8",
    "[Region]" => "8",
    "[TagProtocol]" => "8",
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
            $errorCodes{$1} = $comment;
            $comment = "";
        } else {
            $comment = "";
        }
    }
    close F;
}

sub parseParamComment {
    my ($c) = @_;
    if ($c =~ /^(\"[^\"]+\"),\s+(\w+)/) {
        my ($quoted, $type) = ($1, $2);
        my $haskellType;
        if (exists $toHaskellType{$type}) {
            $haskellType = $toHaskellType{$type};
        } else {
            $haskellType = $nyi;
        }
        return ($haskellType, "$quoted, $haskellType");
    } else {
        return ("", $c);
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
                my ($t, $c) = parseParamComment ($comment);
                $params{$p} = $c;
                $paramType{$p} = $t;
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
            $glueTypes{$1} = $comment;
            $comment = "";
        } elsif (m%^/\*\*\s*(.*?)\s*\*/%) {
            $comment = $1;
        } elsif (/^#define (ERROR_[A-Z0-9_]+)\s+/) {
            push @glueCodes, $1;
            $glueCodes{$1} = $comment;
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
            $regions{$2} = $1;
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
        } elsif (/^\s+(\w+)\s+(\w+)[\w\[\]]*?;/ and defined $structName) {
            my ($fieldName, $type) = ($2, $1);
            push @{$structs->{$structName}{'fields'}}, $fieldName;
            $structs->{$structName}{'type'}{$fieldName} = $type;
            $structs->{$structName}{'comment'}{$fieldName} = $comment;
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
    readStructs ("$apiDir/tmr_gen2.h", \%gen2Structs);
}

sub readGpio {
    readStructs ("$apiDir/tmr_gpio.h", \%gpioStructs);
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
    open F, ">$outputFile" or die;
    for my $line (@lines) {
        print F "$line\n";
    }
    close F;
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
        return (" -- ^ " . escapeHaddock ($x));
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

    emit "data Param =";
    emitEnum (\@params, \%params);
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

sub emitStruct2 {
    my ($hType, $prefix, $cType, $fields, $info) = @_;

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
    my $sep = "{";
    foreach my $field (@$fields) {
        if (exists $info->{$field}) {
            my $fieldType = $info->{$field}{"type"};
            my $comment = $info->{$field}{"comment"};
            my $ufield = ucfirst ($field);
            croak "comment undefined for $field" if (not defined $comment);
            $comment = makeComment ($comment);
            emit "  $sep $prefix$ufield :: $bang($fieldType)$comment";
            $sep = ",";
        }
    }
    emit "  } deriving (Eq, Ord, Show)";
    emit "";

    emit "instance Storable $hType where";
    emit "  sizeOf _ = #{size $cType}";
    emit "  alignment _ = 8"; # because "#alignment" doesn't work for me
    emit "";
    emit "  peek p = do";
    my %already;
    $sep = "let";
    foreach my $field (@$fields) {
        if (exists $info->{$field}) {
            my $c = $info->{$field}{"c"};
            foreach my $cField (@$c) {
                if (not exists $already{$cField}) {
                    my $pfield = makePfield ($cField);
                    emit "    $sep $pfield = #{ptr $cType, $cField} p";
                    $sep = "   ";
                    $already{$cField} = 1;
                }
            }
        }
    }
    emit "    $hType";
    $sep = '<$>';
    foreach my $field (@$fields) {
        if (exists $info->{$field}) {
            my $c = $info->{$field}{"c"};
            my $marshall = $info->{$field}{"marshall"}[0];
            my @ptrs = map { makePfield $_ } @$c;
            emit ("      $sep ($marshall " . join (" ", @ptrs) . ")");
            $sep = '<*>';
        }
    }
    emit "";
    emit '  poke p x = error "poke not implemented"';
    emit "";
}

sub byteStringArrayField {
    my ($fields, $arrayField, $lengthField) = @_;

    my $info = $fields->{$arrayField};
    $info->{"c"} = [$arrayField, $lengthField];
    $info->{"type"} = "ByteString";
    $info->{"marshall"} = ["peekArrayAsByteString", "pokeArrayAsByteString"];

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
    my $oldPeek = $info->{"marshall"}[0];
    my $oldPoke = $info->{"marshall"}[1];
    $info->{"marshall"} = ["$peekWrap <\$> $oldPeek",
                           "$oldPoke \$ $pokeWrap"];
}

sub split64Field {
    my ($fieldOrder, $fields, $field, $comment) = @_;

    my ($lo, $hi) = ("${field}Low", "${field}High");

    my %info;
    $info{"c"} = [$lo, $hi];
    $info{"type"} = "Word64";
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

sub emitGen2 {
    emit "-- | Gen2-specific per-tag data";

    my $cName = "TMR_GEN2_TagData";
    my $cStruct = $gen2Structs{$cName};

    my @fieldOrder;
    my %fields;

    convertStruct ($cStruct, \@fieldOrder, \%fields);

    byteStringArrayField (\%fields, "pc", "pcByteCount");

    emitStruct2 ("GEN2_TagData", "g2", $cName, \@fieldOrder, \%fields);
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
    wrapField (\%fields, "protocol", "toTagProtocol", "fromTagProtocol");

    emitStruct2 ("TagData", "td", $cName, \@fieldOrder, \%fields);
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

    emitStruct2 ("GpioPin", "gp", $cName, \@fieldOrder, \%fields);
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
    # metadataFlags as list of flags
    wrapField (\%fields, "metadataFlags", "unpackFlags16", "packFlags16");
    $fields{"metadataFlags"}{"type"} = "[MetadataFlag]";

    # gpio/gpioCount as array
    listArrayField (\%fields, "gpio", "gpioCount", "[GpioPin]");

    # timestampLow/timestampHigh merged into one
    split64Field (\@fieldOrder, \%fields, "timestamp",
                  "Absolute time of the read, in milliseconds since 1/1/1970 UTC");

    # uint8Lists as bytestrings
    foreach my $f (qw(data epcMemData tidMemData userMemData reservedMemData)) {
        byteStringListField (\%fields, $f);
    }

    emitStruct2 ("TagReadData", "tr", $cName, \@fieldOrder, \%fields);
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

    emit "data ParamType =";
    emitEnum ([sort values %ptn], {});
    emit "  | ParamTypeUnimplemented";
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

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

    emit "paramTypeDisplay :: ParamType -> Text";
    foreach my $paramType (sort values %toHaskellType) {
        my $name = $ptn{$paramType};
        emit "paramTypeDisplay $name = \"$paramType\"";
    }
    emit "paramTypeDisplay _ = \"$nyi\"";

    foreach my $paramType (sort values %toHaskellType) {
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
}

sub emitTagProtocol {
    emit "type RawTagProtocol = #{type TMR_TagProtocol}";
    emit "";

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

    emit "data MetadataFlag =";
    emitEnum (\@metadataFlags, {});
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "fromMetadataFlag :: MetadataFlag -> RawMetadataFlag";
    emitFrom ("fromMetadataFlag", "TMR_TRD_", \@metadataFlags);
    emit "";
}

readStatus();
readParams();
readGlue();
readRegion();
readTagProtocol();
readTagData();
readGen2();
readGpio();

emitHeader();
emitStructs();
emitStatus();
emitRegion();
emitTagProtocol();
emitMetadataFlags();
emitParams();
emitParamTypes();

dumpOutput();
