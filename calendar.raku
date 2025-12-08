my %bs-data = do for 'calendar_bs.csv'.IO.lines.skip(1) {
    next unless .chars;
    my @f = .split(',').map(*.Int);
    @f[0] => @f[1..12]
}

# Define this at the top level of your script
class BSDate {
    has Int $.year;
    has Int $.month;
    has Int $.day;
}

class NepaliConverter {
    # ---------------------------------------------------------
    # 1. CONFIGURATION & DATA
    # ---------------------------------------------------------
    
    # The English Date corresponding to the start of our BS data
    # Anchor: April 13, 1918 AD = Baisakh 1, 1975 BS
    constant $ANCHOR-AD = Date.new(1918, 4, 13);
    constant $ANCHOR-BS-YEAR = 1975;

    # This represents the CSV data. 
    # Key = BS Year, Value = Array of days for months (Baisakh to Chaitra)
    # NOTE: In a real app, read this from the CSV file.
    

    # ---------------------------------------------------------
    # 2. HELPER METHODS
    # ---------------------------------------------------------

    # Get total days in a specific BS year
    method get-days-in-bs-year(Int $year) {
        return %bs-data{$year}.sum if %bs-data{$year}:exists;
        die "Data for BS Year $year not found.";
    }

    # Get days in a specific BS month (1-12)
    method get-days-in-bs-month(Int $year, Int $month) {
        die "Invalid month: $month" unless 1 <= $month <= 12;
        return %bs-data{$year}[$month - 1]; # Array is 0-indexed
    }

    # ---------------------------------------------------------
    # 3. CONVERSION: ENGLISH (AD) -> NEPALI (BS)
    # ---------------------------------------------------------
    multi method to-bs(Date $ad-date) {
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

        return { year => $current-bs-year, month => $current-bs-month, day => $current-bs-day };
    }

    # ---------------------------------------------------------
    # 4. CONVERSION: NEPALI (BS) -> ENGLISH (AD)
    # ---------------------------------------------------------
    
    # Accepts three arguments (Year, Month, Day)
    multi method to-bs($year, $month, $day where $year.Int && $month.Int && $day.Int)
    {
        # ⚠️ Robustness Check: Convert all inputs to Int to avoid octal issues (04, 013).
        my Int $y = $year.Int;
        my Int $m = $month.Int;
        my Int $d = $day.Int;

        # Create the Raku Date object safely
        my Date $ad-date = Date.new($y, $m, $d);

        # Call the primary conversion multi (dispatching)
        return self.to-bs($ad-date);
    }
    
    multi method to-ad(Int $bs-year, Int $bs-month, Int $bs-day) {
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

    multi method to-ad(BSDate $bs-date) {
        # This acts as the primary dispatcher. It calls Version B (the core logic)
        # using the object's attributes.
        return self.to-ad($bs-date.year, $bs-date.month, $bs-date.day);
    }

    constant @WEEKDAYS = <आइत सोम मंगल बुध बिहि शुक्र शनि>;
    
    # Month names (Index 0 is Baisakh, 11 is Chaitra)
    constant @MONTHS = <बैशाख जेठ असार साउन भदौ असोज कार्तिक मंसिर पुष माघ फागुन चैत>;
    
    # ... (Existing helper and conversion methods) ...
    
    # ---------------------------------------------------------
    # 5. CALENDAR DISPLAY METHOD
    # ---------------------------------------------------------
    method display-calendar(Int $bs-year, Int $bs-month) {
        
        # 1. Get Month Name and Length
        my $month-name = @MONTHS[$bs-month - 1];
        my $days-in-month = self.get-days-in-bs-month($bs-year, $bs-month);
        
        # 2. Find the English Date of the 1st of the BS Month
        # We use the existing to-ad method
        my $start-ad-date = self.to-ad($bs-year, $bs-month, 1);
        
        # 3. Calculate the Weekday of the 1st Day
        # Raku's Date.day-of-week returns 1 (Mon) to 7 (Sun). 
        # We need 0 (Sun) to 6 (Sat).
        # We use the standard convention where Sunday is the first day of the week for Nepali calendar.
        my $start-day-of-week = $start-ad-date.day-of-week % 7; # 0=Sun, 1=Mon, ..., 6=Sat
        
        # 4. Print Header
        say "\n    --- {$month-name} {$bs-year} ---";
        say "" ~ @WEEKDAYS.map({ $_.substr(0, 3) }).join(" "); # Print first 3 chars of weekday names
        say "---------------------------";
        
        # 5. Print Initial Spacing (Blank days before the 1st)
        # This creates the padding for the first week.
        print "    " x $start-day-of-week;
        
        # 6. Print the Days
        for 1..$days-in-month -> $day-num {
            # Print the day number (padded with a space)
            print $day-num.fmt('%3d');
            
            # Check if this is the end of the week (Saturday)
            # The current day index (start-day-of-week + day-num) must be divisible by 7
            if ($start-day-of-week + $day-num) % 7 == 0 {
                say ""; # Newline for the next week
            } else {
                print " "; # Space between days
            }
        }
        
        say "\n---------------------------";
    }
}

# ---------------------------------------------------------
# 5. USAGE EXAMPLES
# ---------------------------------------------------------
my $converter = NepaliConverter.new;

# Example 1: Check the Anchor (Should be 1975-01-01)
# my $check-anchor = $converter.to-bs(1918,"04",15);#Date.new(1918, 4, 13));
# say "$check-anchor<year>-$check-anchor<month>-$check-anchor<day>";

$converter.display-calendar(2082, 9);

say $converter.to-ad(1999, 7, 25)
