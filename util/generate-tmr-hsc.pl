#!/usr/bin/perl -w

use strict;
use Carp;
use FindBin;

my $utilDir = $FindBin::Bin;
my $rootDir = "$utilDir/..";
my $apiDir = "$rootDir/cbits/api";
my $glueDir = "$rootDir/cbits/glue";
my $outputDir = "$rootDir/src/System/Hardware/MercuryApi";
my $enumFile = "$outputDir/Enums.hsc";
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

my @opcodes = ();

my @powerModes = ();

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
    "TMR_GEN2_WriteMode" => "GEN2_WriteMode",
    "TMR_SR_PowerMode" => "PowerMode",
    "TMR_TRD_MetadataFlag" => "[MetadataFlag]",
    "TMR_String" => "Text",
    "TMR_uint8List"  => "[Word8]",
  # "TMR_uint16List" => "[Word16]",
    "TMR_uint32List" => "[Word32]",
  # "TMR_int8List"   => "[Int8]",
    "TMR_RegionList" => "[Region]",
    "TMR_TagProtocolList" => "[TagProtocol]",
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

# types which are not in scope in Enums.hs, so need to be linked
# with the full module name
my %needFullLink = map { $_ => 1 } (
    qw(AntennaPort PinNumber GEN2_Password ReadPlan)
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

sub linkifyType {
    my ($type) = @_;

    if (exists $needFullLink{$type}) {
        return "'System.Hardware.MercuryApi.$type'";
    } else {
        return "'$type'";
    }
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
        $linkedType =~ s/(\w+)/linkifyType($1)/e if ($linkedType ne $nyi);
        my $xtra = "";
        if ($linkedType ne $nyi) {
            if (exists $typedefs{$param}) {
                my $typedef = $typedefs{$param};
                $typedef =~ s/(\w+)/linkifyType($1)/e;
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

sub readSerialReaderImp {
    open F, "$apiDir/serial_reader_imp.h" or die;
    while (<F>) {
        if (/^\s+TMR_SR_OPCODE_(\w+)/) {
            push @opcodes, $1;
        }
    }
    close F;
}

sub readSerialReaderH {
    open F, "$apiDir/tmr_serial_reader.h" or die;
    while (<F>) {
        if (/^\s+TMR_SR_(POWER_MODE_\w+)/) {
            my $mode = $1;
            push @powerModes, $mode
                if ($mode ne "POWER_MODE_MIN" and
                    $mode ne "POWER_MODE_MAX" and
                    $mode ne "POWER_MODE_INVALID");
        }
    }
    close F;
}

sub emit {
    my ($s) = @_;
    push @lines, $s;
}

sub emitEnumHeader {
    emit "-- Automatically generated by util/generate-tmr-hsc.pl";
    emit '{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveDataTypeable #-}';
    emit 'module System.Hardware.MercuryApi.Enums where';
    emit '';
    emit 'import Control.Applicative';
    emit 'import Control.Exception';
    emit 'import Data.Hashable';
    emit 'import Data.ByteString (ByteString)';
    emit 'import qualified Data.ByteString as B';
    emit 'import Data.Maybe';
    emit 'import Data.Monoid';
    emit 'import Data.Text (Text)';
    emit 'import qualified Data.Text as T';
    emit 'import qualified Data.Text.Encoding as T';
    emit 'import qualified Data.Text.Encoding.Error as T';
    emit 'import Data.Typeable';
    emit 'import Data.Word';
    emit 'import Foreign';
    emit 'import Foreign.C';
    emit 'import Text.Printf';
    emit '';
    emit '#include <tm_reader.h>';
    emit '#include <serial_reader_imp.h>';
    emit '#include <glue.h>';
    emit '';
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

sub emitOpcodes {
    emit "opcodeName :: Word8 -> Text";
    foreach my $opcode (@opcodes) {
        emit "opcodeName #{const TMR_SR_OPCODE_$opcode} = \"$opcode\"";
    }
    emit 'opcodeName x = "Unknown opcode " <> T.pack (printf "0x%02X" x)';
    emit '';
}

sub emitWriteMode {
    my @modes = ("GEN2_WORD_ONLY", "GEN2_BLOCK_ONLY", "GEN2_BLOCK_FALLBACK");

    emit "type RawWriteMode = #{type TMR_GEN2_WriteMode}";
    emit "";

    emit "-- | Whether to use word write or block write for";
    emit "-- 'System.Hardware.MercuryApi.TagOp_GEN2_WriteData'.";
    emit "data GEN2_WriteMode =";
    emitEnum (\@modes, {});
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "fromWriteMode :: GEN2_WriteMode -> RawWriteMode";
    emitFrom ("fromWriteMode", "TMR_", \@modes);
    emit "";

    emit "toWriteMode :: RawWriteMode -> GEN2_WriteMode";
    emitTo ("toWriteMode", "TMR_", \@modes);
    emit "toWriteMode x = error \$ \"didn't expect WriteMode to be \" ++ show x";
    emit "";
}

sub emitPowerMode {
    emit "type RawPowerMode = #{type TMR_SR_PowerMode}";
    emit "";

    emit "-- | Value for parameter 'PARAM_POWERMODE'.  On the M6e Nano,";
    emit "-- 'POWER_MODE_MINSAVE', 'POWER_MODE_MEDSAVE', and";
    emit "-- 'POWER_MODE_MAXSAVE' are all the same.";
    emit "data PowerMode =";
    emitEnum (\@powerModes, {});
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "fromPowerMode :: PowerMode -> RawPowerMode";
    emitFrom ("fromPowerMode", "TMR_SR_", \@powerModes);
    emit "";

    emit "toPowerMode :: RawPowerMode -> PowerMode";
    emitTo ("toPowerMode", "TMR_SR_", \@powerModes);
    emit "toPowerMode x = error \$ \"didn't expect PowerMode to be \" ++ show x";
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
    emit 'Portability : POSIX, Windows';
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

# Read various .h and .c files

readStatus();
readStrerror();
readSerialReader(); # must be before readParams()
readParams();
readGlue();
readRegion();
readTagProtocol();
readTagData();
readGen2();
readSerialReaderImp();
readSerialReaderH();

# write Enums.hsc

emitEnumHeader();
emitStatus();
emitRegion();
emitTagProtocol();
emitMetadataFlags();
emitBanks();
emitLockBits();
emitOpcodes();
emitWriteMode();
emitPowerMode();
emitParams();
emitParamTypes();

dumpOutput ($enumFile);

# write Params.hs

emitParamHeader();
emitParamHelpers();
emitParamStringHelpers();

dumpOutput ($paramFile);
