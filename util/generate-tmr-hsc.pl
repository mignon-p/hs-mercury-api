#!/usr/bin/perl -w

my $apiDir = ".";
my @errorTypes = ("SUCCESS_TYPE");
my @errorCodes = ("SUCCESS");
my %errorCodes = ("SUCCESS" => "Success!");

sub readStatus {
    open F, "$apiDir/tmr_status.h" or die;
    my $comment = "";
    while (<F>) {
        if (/^#define TMR_(ERROR_TYPE_[A-Z]+)/) {
            push @errorTypes, $1;
        } elsif (m%^/\*\*(.*?)\s+\*/%) {
            $comment = $1;
        } elsif (/^#define TMR_(ERROR_[A-Z_]+)\s+/) {
            push @errorCodes, $1;
            $errorCodes{$1} = $comment;
            $comment = "";
        } else {
            $comment = "";
        }
    }
    close F;
}

sub emit {
    my ($s) = @_;
    print $s, "\n";
}

sub emitEnum {
    my ($constructors, $comments) = @_;
    my $sep = " ";
    for my $con (@$constructors) {
        my $comment = "";
        if (exists $comments->{$con} and $comments->{$con} ne "") {
            $comment = " -- ^ " . $comments->{$con};
        }
        emit "  $sep $con$comment";
        $sep = "|";
    }
}

sub emitTo {
    my ($func, $prefix, $constructors) = @_;
    for my $con (@$constructors) {
        emit "$func #{const $prefix$con} = $con";
    }
}

sub emitStatus {
    emit "data StatusType =";
    emitEnum (\@errorTypes, {});
    emit "  | ERROR_TYPE_UNKNOWN -- ^ Not a recognized status type";
    emit "  deriving (Eq, Ord, Show, Read, Bounded, Enum)";
    emit "";

    emit "toStatusType :: Word32 -> StatusType";
    emitTo ("toStatusType", "TMR_", \@errorTypes);
    emit "toStatusType _ = ERROR_TYPE_UNKNOWN";
    emit "";

    emit "data Status =";
    emitEnum (\@errorCodes, \%errorCodes);
    emit "  | ERROR_UNKNOWN Word32 -- ^ C API returned an unrecognized status code";
    emit "  deriving (Eq, Ord, Show, Read)";
    emit "";

    emit "toStatus :: Word32 -> Status";
    emitTo ("toStatus", "TMR_", \@errorCodes);
    emit "toStatus x = ERROR_UNKNOWN x";
    emit "";
}

readStatus();
emitStatus();
