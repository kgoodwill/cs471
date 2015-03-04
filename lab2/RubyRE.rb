# RubyREm.rb -- Thanks to J Marshall 2011

class Test
  # Constructor
  def initialize(pattern, output_file)
    # instance variables
    @pattern = pattern  
    @output_file = output_file
  end

  def read_and_parse_from input_file
    # Open output file and associates a block code following  "do block |output|"
    File.open @output_file, 'w' do |output|
      # inspect inherited from Object 
      output.puts "Info from #{input_file} matching group two of #{@pattern.inspect}"
      # File.open not only opens a file and associates a block of code -- do block |input|  ...
      #  File.open also opens input_file as an iterator 
      File.open input_file do |input|
      # iterating each line in input_file
        input.each do |line|
          if match = @pattern.match(line)
            # if  RE matches the line, match is the array returned.  Print out the 2nd "group"
            output.puts "    "
            output.puts match[2]
            # total RE match
            output.puts "match #{match}"
            # more examples
            output.puts  "match[0]::: #{match[0]}"
            output.puts  "match[1]::: #{match[1]} \n"
          end
        end
      end
    end
  end
end


# literal re is "/(int)\s(.*)\(.*\);/"
# \s  space (0x20), ")" is a meta character, "\)" will match the character ")", "." and "*" are meta character
# Group 2's pattern matches all characters that follow "int" keyword and terminates with  "("
# What does group 1 match?  What does group 0 match?

Test.new(/(int)\s(.*)\(.*\);/, "Names.txt").read_and_parse_from "Fig4_12.c"
Test.new(/match\('(.)'\)/, "output.txt").read_and_parse_from "Fig4_12.c"