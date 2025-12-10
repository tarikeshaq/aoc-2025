let () = 
    let sample_lines = (Fileloader.load_file "sample.txt") in
    let input_lines = (Fileloader.load_file "input.txt") in
    Printf.printf "Sample part1: %d\n" (Day9.part1 sample_lines);
    Printf.printf "Solution part 1: %d\n" (Day9.part1 input_lines);
    Printf.printf "Sample part2: %d\n" (Day9.part2 sample_lines);
    Printf.printf "Solution part 2: %d\n" (Day9.part2 input_lines);
