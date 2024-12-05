use std::fs::File;
use std::io::{prelude::*, BufReader};
use std::env;

struct Rule {
    first: u32,
    second: u32,
}

impl Rule {
    fn applies(&self, order: &Vec<u32>) -> bool {
        let first = order.iter().rposition(|&n| n == self.first);
        let second = order.iter().position(|&n| n == self.second);
        if first == None || second == None {
            return true;
        } else {
            return Some(first) < Some(second);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 1 {
        return;
    }
    let f = File::open(&args[args.len() - 1]).unwrap();
    let reader = BufReader::new(f);

    let mut parsed_rules = false;
    let mut rules: Vec<Rule> = Vec::new();
    let mut accum = 0;

    for l in reader.lines() {
        let line = l.unwrap();
        if line == "" {
            parsed_rules = true;
            continue;
        }

        if !parsed_rules {
            let nums = line.split("|").collect::<Vec<&str>>();
            rules.push(Rule {
                first: nums[0].parse::<u32>().unwrap(),
                second: nums[1].parse::<u32>().unwrap()
            });
        } else {
            let order: Vec<u32> = line.split(",").map(|n| n.parse::<u32>().unwrap()).collect();
            let mut correct = true;
            for rule in rules.iter() {
                if !rule.applies(&order) {
                    correct = false;
                    break;
                }
            }

            if correct {
                accum += order[order.len() / 2];
            }
        }
    }
    println!("{}", accum);
}
