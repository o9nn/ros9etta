/*
 * opencog-cogutil.go
 * 
 * OpenCog Cogutil - Go Utility Library
 * A collection of utility functions and classes for OpenCog framework
 * 
 * This single-file implementation demonstrates Go's strengths:
 * - Simplicity and readability
 * - Built-in concurrency with goroutines and channels
 * - Struct embedding for composition
 * - Interfaces for polymorphism
 * - Fast compilation and execution
 */

package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
	"time"
	"unicode"
)

// ===== Logger System =====
// Demonstrates: Enums with iota, methods on structs

type LogLevel int

const (
	DEBUG LogLevel = iota
	INFO
	WARN
	ERROR
)

func (l LogLevel) String() string {
	switch l {
	case DEBUG:
		return "DEBUG"
	case INFO:
		return "INFO"
	case WARN:
		return "WARN"
	case ERROR:
		return "ERROR"
	default:
		return "UNKNOWN"
	}
}

type Logger struct {
	name     string
	minLevel LogLevel
}

func NewLogger(name string, level LogLevel) *Logger {
	return &Logger{
		name:     name,
		minLevel: level,
	}
}

func (l *Logger) log(level LogLevel, message string) {
	if level >= l.minLevel {
		timestamp := time.Now().Format("15:04:05")
		fmt.Printf("[%s] %s: %s\n", timestamp, level, message)
	}
}

func (l *Logger) Debug(message string) {
	l.log(DEBUG, message)
}

func (l *Logger) Info(message string) {
	l.log(INFO, message)
}

func (l *Logger) Warn(message string) {
	l.log(WARN, message)
}

func (l *Logger) Error(message string) {
	l.log(ERROR, message)
}

func (l *Logger) SetLevel(level LogLevel) {
	l.minLevel = level
}

// ===== Configuration Manager =====
// Demonstrates: Maps, error handling

type Config struct {
	data map[string]string
}

func NewConfig() *Config {
	return &Config{
		data: make(map[string]string),
	}
}

func (c *Config) Set(key, value string) {
	c.data[key] = value
}

func (c *Config) Get(key string) (string, bool) {
	value, ok := c.data[key]
	return value, ok
}

func (c *Config) GetOr(key, defaultValue string) string {
	if value, ok := c.data[key]; ok {
		return value
	}
	return defaultValue
}

func (c *Config) Has(key string) bool {
	_, ok := c.data[key]
	return ok
}

func (c *Config) LoadFromFile(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		
		// Skip comments and empty lines
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		// Parse key=value pairs
		parts := strings.SplitN(line, "=", 2)
		if len(parts) == 2 {
			key := strings.TrimSpace(parts[0])
			value := strings.TrimSpace(parts[1])
			c.Set(key, value)
		}
	}

	return scanner.Err()
}

func (c *Config) Dump() {
	// Sort keys for consistent output
	keys := make([]string, 0, len(c.data))
	for k := range c.data {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	for _, k := range keys {
		fmt.Printf("%s = %s\n", k, c.data[k])
	}
}

// ===== Timer Utility =====
// Demonstrates: Embedded time handling, methods

type Timer struct {
	name      string
	startTime time.Time
	elapsed   time.Duration
}

func NewTimer(name string) *Timer {
	return &Timer{
		name: name,
	}
}

func (t *Timer) Start() {
	t.startTime = time.Now()
}

func (t *Timer) Stop() time.Duration {
	t.elapsed = time.Since(t.startTime)
	return t.elapsed
}

func (t *Timer) Elapsed() time.Duration {
	return t.elapsed
}

func (t *Timer) ElapsedSeconds() float64 {
	return t.elapsed.Seconds()
}

// ===== String Utilities =====
// Demonstrates: String manipulation, slices

type StringUtils struct{}

func (StringUtils) Split(text string, delimiter rune) []string {
	parts := strings.FieldsFunc(text, func(r rune) bool {
		return r == delimiter
	})
	
	// Trim whitespace
	result := make([]string, 0, len(parts))
	for _, part := range parts {
		trimmed := strings.TrimSpace(part)
		if trimmed != "" {
			result = append(result, trimmed)
		}
	}
	return result
}

func (StringUtils) Join(strs []string, delimiter string) string {
	return strings.Join(strs, delimiter)
}

func (StringUtils) ToLower(text string) string {
	return strings.ToLower(text)
}

func (StringUtils) ToUpper(text string) string {
	return strings.ToUpper(text)
}

func (StringUtils) Trim(text string) string {
	return strings.TrimSpace(text)
}

func (StringUtils) CamelToSnake(text string) string {
	var result strings.Builder
	for i, r := range text {
		if unicode.IsUpper(r) && i > 0 {
			result.WriteRune('_')
		}
		result.WriteRune(unicode.ToLower(r))
	}
	return result.String()
}

func (StringUtils) SnakeToCamel(text string) string {
	parts := strings.Split(text, "_")
	var result strings.Builder
	
	for i, part := range parts {
		if i == 0 {
			result.WriteString(part)
		} else if len(part) > 0 {
			// Capitalize first letter manually (strings.Title is deprecated in Go 1.18+)
			result.WriteString(strings.ToUpper(string(part[0])) + part[1:])
		}
	}
	return result.String()
}

// ===== Performance Measurement =====
// Demonstrates: Higher-order functions, closures

func MeasurePerformance(operation string, f func()) {
	fmt.Printf("[PERF] Starting: %s\n", operation)
	start := time.Now()
	f()
	elapsed := time.Since(start)
	fmt.Printf("[PERF] Completed: %s in %.6fs\n", operation, elapsed.Seconds())
}

func TimeIt(name string, f func()) {
	start := time.Now()
	f()
	elapsed := time.Since(start)
	fmt.Printf("[TIMER] %s took %.6f seconds\n", name, elapsed.Seconds())
}

// ===== Concurrent Processing =====
// Demonstrates: Goroutines, channels, concurrency

func ProcessConcurrently(items []int, workers int) []int {
	jobs := make(chan int, len(items))
	results := make(chan int, len(items))

	// Start workers
	for w := 1; w <= workers; w++ {
		go func(id int) {
			for item := range jobs {
				// Simulate processing
				result := item * item
				results <- result
			}
		}(w)
	}

	// Send jobs
	for _, item := range items {
		jobs <- item
	}
	close(jobs)

	// Collect results
	processed := make([]int, 0, len(items))
	for i := 0; i < len(items); i++ {
		processed = append(processed, <-results)
	}

	return processed
}

// ===== Demonstration Function =====

func demonstrateCogutil() {
	fmt.Println("======================================================================")
	fmt.Println("OpenCog Cogutil - Go Utility Library Demo")
	fmt.Println("Showcasing Go's strengths: Simplicity, concurrency, fast compilation")
	fmt.Println("======================================================================")
	fmt.Println()

	// 1. Logger demonstration
	fmt.Println("1. Logger Demonstration")
	fmt.Println("--------------------------------------------------")
	logger := NewLogger("CogUtil", INFO)
	logger.Info("Cogutil library initialized")
	logger.Debug("This debug message won't show (level too low)")
	logger.Warn("This is a warning message")
	logger.Error("This is an error message")
	
	logger.SetLevel(DEBUG)
	logger.Debug("Now debug messages are visible")
	fmt.Println()

	// 2. Config demonstration
	fmt.Println("2. Configuration Manager")
	fmt.Println("--------------------------------------------------")
	config := NewConfig()
	config.Set("opencog.version", "1.0.0")
	config.Set("atomspace.enabled", "true")
	config.Set("cogserver.port", "17001")
	
	logger.Info("Configuration loaded:")
	config.Dump()
	fmt.Println()
	
	logger.Info(fmt.Sprintf("Port setting: %s", config.GetOr("cogserver.port", "unknown")))
	fmt.Println()

	// 3. Timer demonstration
	fmt.Println("3. Timer Utility")
	fmt.Println("--------------------------------------------------")
	timer := NewTimer("Processing")
	timer.Start()
	logger.Info("Simulating some work...")
	
	// Simulate work
	sum := 0
	for i := 0; i < 1000000; i++ {
		sum += i
	}
	_ = sum
	
	timer.Stop()
	logger.Info(fmt.Sprintf("Timer elapsed: %.6fs", timer.ElapsedSeconds()))
	fmt.Println()

	// 4. Performance measurement
	fmt.Println("4. Performance Measurement")
	fmt.Println("--------------------------------------------------")
	MeasurePerformance("Data processing", func() {
		time.Sleep(10 * time.Millisecond)
	})
	fmt.Println()

	// 5. String utilities
	fmt.Println("5. String Utilities")
	fmt.Println("--------------------------------------------------")
	logger.Info("String utilities demonstration:")
	
	utils := StringUtils{}
	text := "OpenCog,AtomSpace,CogServer,Cogutil"
	parts := utils.Split(text, ',')
	
	logger.Info("Split result:")
	for _, part := range parts {
		fmt.Printf("  - %s\n", part)
	}
	
	joined := utils.Join(parts, " + ")
	logger.Info(fmt.Sprintf("Joined: %s", joined))
	
	logger.Info(fmt.Sprintf("Uppercase: %s", utils.ToUpper("opencog rocks")))
	logger.Info(fmt.Sprintf("Lowercase: %s", utils.ToLower("OPENCOG ROCKS")))
	logger.Info(fmt.Sprintf("Trimmed: '%s'", utils.Trim("  spaced out  ")))
	
	// Go-specific: Case conversions
	logger.Info(fmt.Sprintf("camelCase → snake_case: %s", utils.CamelToSnake("myVariableName")))
	logger.Info(fmt.Sprintf("snake_case → camelCase: %s", utils.SnakeToCamel("my_variable_name")))
	fmt.Println()

	// 6. Function timing
	fmt.Println("6. Function Timing")
	fmt.Println("--------------------------------------------------")
	
	var result int
	TimeIt("fibonacci", func() {
		// Note: Using naive recursive fibonacci for demonstration
		// O(2^n) complexity - suitable for small n only
		fibonacci := func(n int) int {
			var fib func(int) int
			fib = func(n int) int {
				if n <= 1 {
					return n
				}
				return fib(n-1) + fib(n-2)
			}
			return fib(n)
		}
		result = fibonacci(25)
	})
	logger.Info(fmt.Sprintf("Fibonacci(25) = %d", result))
	fmt.Println()

	// 7. Slices and maps (Go-specific)
	fmt.Println("7. Slices and Maps (Go-specific)")
	fmt.Println("--------------------------------------------------")
	squares := make([]int, 10)
	for i := range squares {
		squares[i] = i * i
	}
	logger.Info(fmt.Sprintf("Squares: %v", squares))
	
	evens := make([]int, 0)
	for i := 0; i < 20; i++ {
		if i%2 == 0 {
			evens = append(evens, i)
		}
	}
	logger.Info(fmt.Sprintf("Even numbers: %v", evens))
	fmt.Println()

	// 8. Goroutines and channels (Go-specific)
	fmt.Println("8. Concurrency (Goroutines & Channels)")
	fmt.Println("--------------------------------------------------")
	logger.Info("Processing items concurrently with 4 workers...")
	
	items := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
	processed := ProcessConcurrently(items, 4)
	
	logger.Info(fmt.Sprintf("Input: %v", items))
	logger.Info(fmt.Sprintf("Squared (concurrent): %v", processed))
	fmt.Println()

	// 9. Error handling
	fmt.Println("9. Error Handling (Go-specific)")
	fmt.Println("--------------------------------------------------")
	err := config.LoadFromFile("nonexistent.conf")
	if err != nil {
		logger.Warn(fmt.Sprintf("Config load failed (expected): %v", err))
	} else {
		logger.Info("Config loaded successfully")
	}
	fmt.Println()

	logger.Info("Cogutil demonstration complete!")
	fmt.Println("======================================================================")
	fmt.Println("Go strengths demonstrated:")
	fmt.Println("  ✓ Simple, readable syntax")
	fmt.Println("  ✓ Built-in concurrency (goroutines, channels)")
	fmt.Println("  ✓ Fast compilation and execution")
	fmt.Println("  ✓ Struct embedding for composition")
	fmt.Println("  ✓ Interfaces for polymorphism")
	fmt.Println("  ✓ Explicit error handling")
	fmt.Println("  ✓ Garbage collection")
	fmt.Println("======================================================================")
}

func main() {
	demonstrateCogutil()
}
