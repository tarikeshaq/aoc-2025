let () = 
    let sample_lines = (Fileloader.load_file "sample.txt") in
    let input_lines = (Fileloader.load_file "input.txt") in
    Printf.printf "Sample part1: %s\n" (Day1.part1 sample_lines);
    Printf.printf "Sample part2: %s\n" (Day1.part2 sample_lines);
    Printf.printf "Solution part 1: %s\n" (Day1.part1 input_lines);
    Printf.printf "Solution part 2: %s\n" (Day1.part2 input_lines);
