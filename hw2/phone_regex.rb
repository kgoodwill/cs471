#Multiple phone numbers on the same line are not accepted
class PhoneRegex
  def initialize(pattern, match_output_file, number_output_file)
    @pattern = pattern
    @match_output_file = match_output_file
		@number_output_file = number_output_file
  end

  def read_and_parse_from input_file
    match_output = File.open @match_output_file, 'w'
    number_output = File.open @number_output_file, 'w'
    match_output.puts "Info from #{input_file} matching any group of #{@pattern.inspect}"
      File.open input_file do |input|
        input.each do |line|
          if match = @pattern.match(line)
            match_output.puts ' '
            match_output.puts match[2]
            match_output.puts "match #{match}"
            number_output.puts ' '
            number_output.puts "#{match}"
          end
        end
      end
  end
end

PhoneRegex.new(/\+1 [2-9][0-9]{2} - [2-9][0-9]{2} - [0-9]{4}|\+1 \([2-9][0-9]{2}\) [2-9][0-9]{2} - [0-9]{4}/, 'PatOP1.txt', 'PhoneOP1.txt').read_and_parse_from "phone_numbers.txt"

PhoneRegex.new(/\+1 [2-9][0-9]{2} - [2-9][0-9]{2} - [0-9]{4}|\+1 \([2-9][0-9]{2}\) [2-9][0-9]{2} - [0-9]{4}/, 'PatOP.txt', 'PhoneOP.txt').read_and_parse_from "PhoneIP.txt"


