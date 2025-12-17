/*
 * opencog-cogutil.rs
 * 
 * OpenCog Cogutil - Rust Utility Library
 * A collection of utility functions and classes for OpenCog framework
 * 
 * This single-file implementation demonstrates Rust's strengths:
 * - Memory safety without garbage collection
 * - Ownership and borrowing for safe concurrency
 * - Zero-cost abstractions
 * - Pattern matching and Result/Option types
 * - Trait-based polymorphism
 */

use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Write};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use std::fmt;

// ===== Logger System =====
// Demonstrates: Enums, pattern matching, trait implementation

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum LogLevel {
    Debug,
    Info,
    Warn,
    Error,
}

impl fmt::Display for LogLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogLevel::Debug => write!(f, "DEBUG"),
            LogLevel::Info => write!(f, "INFO"),
            LogLevel::Warn => write!(f, "WARN"),
            LogLevel::Error => write!(f, "ERROR"),
        }
    }
}

struct Logger {
    name: String,
    min_level: LogLevel,
}

impl Logger {
    fn new(name: &str, level: LogLevel) -> Self {
        Logger {
            name: name.to_string(),
            min_level: level,
        }
    }

    fn log(&self, level: LogLevel, message: &str) {
        if level >= self.min_level {
            let timestamp = Self::get_timestamp();
            println!("[{}] {}: {}", timestamp, level, message);
        }
    }

    fn debug(&self, message: &str) {
        self.log(LogLevel::Debug, message);
    }

    fn info(&self, message: &str) {
        self.log(LogLevel::Info, message);
    }

    fn warn(&self, message: &str) {
        self.log(LogLevel::Warn, message);
    }

    fn error(&self, message: &str) {
        self.log(LogLevel::Error, message);
    }

    fn set_level(&mut self, level: LogLevel) {
        self.min_level = level;
    }

    fn get_timestamp() -> String {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap();
        let secs = now.as_secs();
        let hours = (secs / 3600) % 24;
        let mins = (secs / 60) % 60;
        let secs = secs % 60;
        format!("{:02}:{:02}:{:02}", hours, mins, secs)
    }
}

// ===== Configuration Manager =====
// Demonstrates: HashMap, ownership, Result type

struct Config {
    data: HashMap<String, String>,
}

impl Config {
    fn new() -> Self {
        Config {
            data: HashMap::new(),
        }
    }

    fn set(&mut self, key: String, value: String) {
        self.data.insert(key, value);
    }

    fn get(&self, key: &str) -> Option<&String> {
        self.data.get(key)
    }

    fn get_or(&self, key: &str, default: &str) -> String {
        self.data.get(key).map(|s| s.clone()).unwrap_or_else(|| default.to_string())
    }

    fn has(&self, key: &str) -> bool {
        self.data.contains_key(key)
    }

    fn load_from_file(&mut self, filename: &str) -> Result<(), io::Error> {
        let file = File::open(filename)?;
        let reader = BufReader::new(file);

        for line in reader.lines() {
            let line = line?;
            let trimmed = line.trim();
            
            // Skip comments and empty lines
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }

            // Parse key=value pairs
            if let Some(pos) = trimmed.find('=') {
                let key = trimmed[..pos].trim().to_string();
                let value = trimmed[pos + 1..].trim().to_string();
                self.set(key, value);
            }
        }

        Ok(())
    }

    fn dump(&self) {
        let mut keys: Vec<_> = self.data.keys().collect();
        keys.sort();
        
        for key in keys {
            println!("{} = {}", key, self.data[key]);
        }
    }
}

// ===== Timer Utility =====
// Demonstrates: RAII pattern, Option type, method chaining

struct Timer {
    name: String,
    start_time: Option<Instant>,
    elapsed: Option<Duration>,
}

impl Timer {
    fn new(name: &str) -> Self {
        Timer {
            name: name.to_string(),
            start_time: None,
            elapsed: None,
        }
    }

    fn start(&mut self) {
        self.start_time = Some(Instant::now());
    }

    fn stop(&mut self) -> Duration {
        if let Some(start) = self.start_time {
            let elapsed = start.elapsed();
            self.elapsed = Some(elapsed);
            elapsed
        } else {
            Duration::from_secs(0)
        }
    }

    fn elapsed(&self) -> Option<Duration> {
        self.elapsed
    }

    fn elapsed_secs(&self) -> f64 {
        self.elapsed.map(|d| d.as_secs_f64()).unwrap_or(0.0)
    }
}

// ===== String Utilities =====
// Demonstrates: String handling, iterators, functional programming

struct StringUtils;

impl StringUtils {
    fn split(text: &str, delimiter: char) -> Vec<String> {
        text.split(delimiter)
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
            .collect()
    }

    fn join(strings: &[String], delimiter: &str) -> String {
        strings.join(delimiter)
    }

    fn to_lower(text: &str) -> String {
        text.to_lowercase()
    }

    fn to_upper(text: &str) -> String {
        text.to_uppercase()
    }

    fn trim(text: &str) -> String {
        text.trim().to_string()
    }

    fn camel_to_snake(text: &str) -> String {
        let mut result = String::new();
        for (i, c) in text.chars().enumerate() {
            if c.is_uppercase() && i > 0 {
                result.push('_');
            }
            result.push(c.to_lowercase().next().unwrap());
        }
        result
    }

    fn snake_to_camel(text: &str) -> String {
        let parts: Vec<&str> = text.split('_').collect();
        let mut result = String::new();
        
        for (i, part) in parts.iter().enumerate() {
            if i == 0 {
                result.push_str(part);
            } else {
                let mut chars = part.chars();
                if let Some(first) = chars.next() {
                    result.push(first.to_uppercase().next().unwrap());
                    result.push_str(&chars.collect::<String>());
                }
            }
        }
        result
    }
}

// ===== Performance Measurement =====
// Demonstrates: Closures, generic functions, higher-order functions

fn measure_performance<F, T>(operation: &str, f: F) -> T
where
    F: FnOnce() -> T,
{
    println!("[PERF] Starting: {}", operation);
    let start = Instant::now();
    let result = f();
    let elapsed = start.elapsed();
    println!("[PERF] Completed: {} in {:.6}s", operation, elapsed.as_secs_f64());
    result
}

// ===== Timing Decorator =====
// Demonstrates: Function wrapping, measurement

fn timeit<F, T>(name: &str, f: F) -> T
where
    F: FnOnce() -> T,
{
    let start = Instant::now();
    let result = f();
    let elapsed = start.elapsed();
    println!("[TIMER] {} took {:.6} seconds", name, elapsed.as_secs_f64());
    result
}

// ===== Demonstration Function =====

fn demonstrate_cogutil() {
    println!("======================================================================");
    println!("OpenCog Cogutil - Rust Utility Library Demo");
    println!("Showcasing Rust's strengths: Memory safety, zero-cost, ownership");
    println!("======================================================================");
    println!();

    // 1. Logger demonstration
    println!("1. Logger Demonstration");
    println!("--------------------------------------------------");
    let logger = Logger::new("CogUtil", LogLevel::Info);
    logger.info("Cogutil library initialized");
    logger.debug("This debug message won't show (level too low)");
    logger.warn("This is a warning message");
    logger.error("This is an error message");
    
    let mut logger_debug = Logger::new("CogUtil", LogLevel::Debug);
    logger_debug.set_level(LogLevel::Debug);
    logger_debug.debug("Now debug messages are visible");
    println!();

    // 2. Config demonstration
    println!("2. Configuration Manager");
    println!("--------------------------------------------------");
    let mut config = Config::new();
    config.set("opencog.version".to_string(), "1.0.0".to_string());
    config.set("atomspace.enabled".to_string(), "true".to_string());
    config.set("cogserver.port".to_string(), "17001".to_string());
    
    logger.info("Configuration loaded:");
    config.dump();
    println!();
    
    logger.info(&format!("Port setting: {}", config.get_or("cogserver.port", "unknown")));
    println!();

    // 3. Timer demonstration
    println!("3. Timer Utility");
    println!("--------------------------------------------------");
    let mut timer = Timer::new("Processing");
    timer.start();
    logger.info("Simulating some work...");
    
    // Simulate work
    let sum: u64 = (0..1_000_000).sum();
    let _ = sum;
    
    timer.stop();
    logger.info(&format!("Timer elapsed: {:.6}s", timer.elapsed_secs()));
    println!();

    // 4. Performance measurement
    println!("4. Performance Measurement");
    println!("--------------------------------------------------");
    measure_performance("Data processing", || {
        std::thread::sleep(Duration::from_millis(10));
    });
    println!();

    // 5. String utilities
    println!("5. String Utilities");
    println!("--------------------------------------------------");
    logger.info("String utilities demonstration:");
    let text = "OpenCog,AtomSpace,CogServer,Cogutil";
    let parts = StringUtils::split(text, ',');
    
    logger.info("Split result:");
    for part in &parts {
        println!("  - {}", part);
    }
    
    let joined = StringUtils::join(&parts, " + ");
    logger.info(&format!("Joined: {}", joined));
    
    logger.info(&format!("Uppercase: {}", StringUtils::to_upper("opencog rocks")));
    logger.info(&format!("Lowercase: {}", StringUtils::to_lower("OPENCOG ROCKS")));
    logger.info(&format!("Trimmed: '{}'", StringUtils::trim("  spaced out  ")));
    
    // Rust-specific: Case conversions
    logger.info(&format!("camelCase → snake_case: {}", 
                        StringUtils::camel_to_snake("myVariableName")));
    logger.info(&format!("snake_case → camelCase: {}", 
                        StringUtils::snake_to_camel("my_variable_name")));
    println!();

    // 6. Timing decorator
    println!("6. Function Timing");
    println!("--------------------------------------------------");
    
    let result = timeit("fibonacci", || {
        fn fibonacci(n: u32) -> u64 {
            if n <= 1 {
                n as u64
            } else {
                fibonacci(n - 1) + fibonacci(n - 2)
            }
        }
        fibonacci(30)
    });
    logger.info(&format!("Fibonacci(30) = {}", result));
    println!();

    // 7. Iterator demonstration (Rust-specific)
    println!("7. Iterators (Rust-specific)");
    println!("--------------------------------------------------");
    let squares: Vec<u32> = (0..10).map(|x| x * x).collect();
    logger.info(&format!("Squares: {:?}", squares));
    
    let evens: Vec<u32> = (0..20).filter(|x| x % 2 == 0).collect();
    logger.info(&format!("Even numbers: {:?}", evens));
    println!();

    // 8. Ownership demonstration
    println!("8. Ownership & Borrowing (Rust-specific)");
    println!("--------------------------------------------------");
    let s1 = String::from("OpenCog");
    let s2 = s1.clone(); // Explicit clone to avoid move
    logger.info(&format!("Original: {}, Clone: {}", s1, s2));
    
    // Borrowing example
    let config_ref = &config;
    logger.info(&format!("Has port config: {}", config_ref.has("cogserver.port")));
    println!();

    // 9. Result type for error handling
    println!("9. Result Type (Error Handling)");
    println!("--------------------------------------------------");
    match config.load_from_file("nonexistent.conf") {
        Ok(_) => logger.info("Config loaded successfully"),
        Err(e) => logger.warn(&format!("Config load failed (expected): {}", e)),
    }
    println!();

    logger.info("Cogutil demonstration complete!");
    println!("======================================================================");
    println!("Rust strengths demonstrated:");
    println!("  ✓ Memory safety without garbage collection");
    println!("  ✓ Ownership and borrowing system");
    println!("  ✓ Zero-cost abstractions");
    println!("  ✓ Pattern matching (enums, Option, Result)");
    println!("  ✓ Trait-based polymorphism");
    println!("  ✓ Powerful iterators");
    println!("  ✓ Compile-time guarantees");
    println!("======================================================================");
}

fn main() {
    demonstrate_cogutil();
}
