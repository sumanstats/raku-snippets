unit module Converter;
class NepaliDateConverter { ... };

class NepaliDate is export {
    has Int $.year;
    has Int $.month;
    has Int $.day;

    multi method new(Int $year, Int $month, Int $day) {
        self.bless(:$year, :$month, :$day);
    }

    multi method new(:$year!, :$month!, :$day!) {
        self.bless(:$year, :$month, :$day);
    }

    submethod BUILD(:$!year, :$!month, :$!day) {
        fail "Invalid year" unless 1975 <= $!year <= 2100;
        fail "Invalid month" unless 1 <= $!month <= 12;
        fail "Invalid day" unless 1 <= $!day <= 32;
    }

    method to-ad() {
        my $converter = NepaliDateConverter.new;
        return $converter.to-ad(self);
    }

    method Str() {
        sprintf "%04d-%02d-%02d", $!year, $!month, $!day;
    }

    method gist() {
        self.Str
    }
}


# This represents the CSV data. 
    # Key = Nepali BS Year, Value = Array of days for months (Baisakh to Chaitra)
    # Read from the CSV file.
    # my %bs_data = (
        # 1975 => [31, 31, 32, 31, 31, 31, 30, 29, 30, 29, 30, 30],
        # 1976 => [31, 32, 31, 32, 31, 30, 30, 30, 29, 29, 30, 31],
        # ... (You would fill this gap with the CSV data) ...
        # 2080 => [31, 32, 31, 32, 31, 30, 30, 30, 29, 29, 30, 30],
        # 2081 => [31, 32, 31, 32, 31, 30, 30, 30, 29, 30, 29, 31],
    # );


class NepaliDateConverter is export {
    # ---------------------------------------------------------
    # 1. CONFIGURATION 
    # ---------------------------------------------------------
    
    # The English Date corresponding to the start of our Nepali BS data
    # Anchor: April 13, 1918 AD = Baisakh 1, 1975 BS
    constant $ANCHOR-AD = Date.new(1918, 4, 13);
    constant $ANCHOR-BS-YEAR = 1975;    

    # This represents the CSV data
    my %bs_data;
    my $data-loaded = False;
    
    # ---------------------------------------------------------
    # 2. DATA LOADING
    # ---------------------------------------------------------
    
    method !load-bs-data() {
        return if $data-loaded;  # Already loaded
        
        # Try to find the CSV file in common locations
        
        %bs_data = do for '../data/calendar_bs.csv'.IO.lines.skip(1) {
            next unless .chars;
            my @f = .split(',').map(*.Int);
            @f[0] => @f[1..12]
        };

        return %bs_data;        
        $data-loaded = True;
    }
    

    # ---------------------------------------------------------
    # 3. HELPER METHODS
    # ---------------------------------------------------------

    # Get total days in a specific BS year
    method get-days-in-bs-year(Int $year) {
        self!load-bs-data(); 
        return %bs_data{$year}.sum if %bs_data{$year}:exists;
        die "Data for Nepali BS Year $year not found.";
    }

    # Get days in a specific BS month (1-12)
    method get-days-in-bs-month(Int $year, Int $month) {
        self!load-bs-data(); 
        die "Invalid month: $month" unless 1 <= $month <= 12;
        return %bs_data{$year}[$month - 1]; # Array is 0-indexed
    }

    # ---------------------------------------------------------
    # 4. CONVERSION: ENGLISH (AD) -> NEPALI (BS)
    # ---------------------------------------------------------
    method to-bs(Date $ad-date) {
        self!load-bs-data(); 
        # Step A: Calculate total days passed since the Anchor
        my $days-passed = $ad-date - $ANCHOR-AD;

        if $days-passed < 0 {
            die "Date is before the supported Anchor date (1918-04-13).";
        }

        my $current-bs-year = $ANCHOR-BS-YEAR;
        my $current-bs-month = 1;
        my $current-bs-day = 1;

        # Step B: Distribute days into Years
        loop {
            my $days-in-year = self.get-days-in-bs-year($current-bs-year);
            if $days-passed < $days-in-year {
                last; # The remaining days fall within this year
            }
            $days-passed -= $days-in-year;
            $current-bs-year++;
        }

        # Step C: Distribute remaining days into Months
        loop {
            my $days-in-month = self.get-days-in-bs-month($current-bs-year, $current-bs-month);
            if $days-passed < $days-in-month {
                # The remaining days are the day of the month
                # +1 because if 0 days passed, it is the 1st of the month
                $current-bs-day = $days-passed + 1;
                last;
            }
            $days-passed -= $days-in-month;
            $current-bs-month++;
        }

        # Return NepaliDate object instead of hash
        return NepaliDate.new($current-bs-year, $current-bs-month, $current-bs-day);
    }

    # ---------------------------------------------------------
    # 5. CONVERSION: NEPALI (BS) -> ENGLISH (AD)
    # ---------------------------------------------------------

    multi method to-ad(NepaliDate $bs-date) {
        self.to-ad($bs-date.year, $bs-date.month, $bs-date.day);
    }

    multi method to-ad(Int $bs-year, Int $bs-month, Int $bs-day) {
        self!load-bs-data(); 
        my $total-days = 0;

        # Step A: Add days for full years passed since Anchor
        loop (my $y = $ANCHOR-BS-YEAR; $y < $bs-year; $y++) {
            $total-days += self.get-days-in-bs-year($y);
        }

        # Step B: Add days for full months passed in current year
        loop (my $m = 1; $m < $bs-month; $m++) {
            $total-days += self.get-days-in-bs-month($bs-year, $m);
        }

        # Step C: Add days passed in current month
        $total-days += ($bs-day - 1); # -1 because 1st of month means 0 days passed

        # Step D: Add total delta to the Anchor AD Date
        return $ANCHOR-AD + $total-days;
    }
}
