// Tidal-Integrated Pitch-Aware Sampler
// Combines the robustness of superdirt-pitch-sampler with native SuperDirt integration
// Based on SuperDough's closest-sample algorithm from Strudel

(
// Global storage for pitch-aware sample banks
~tidalPitchBanks = Dictionary.new;
~tidalPitchBuffers = Dictionary.new; // Buffer cache to avoid reloading

// Enhanced sample bank registration with multiple loading strategies
~registerTidalPitchBank = { |bankName, config, basePath = ""|
    var processedBank = Dictionary.new;
    var loadStrategy = config[\strategy] ? \noteMap; // \noteMap, \autoDetect, or \chromatic
    
    switch(loadStrategy,
        \noteMap, {
            // Explicit note mappings like original superdirt-pitch-sampler
            config[\samples].keysValuesDo { |note, samples|
                var noteMidi = if(note.isKindOf(Symbol)) { 
                    note.asString.notemidi 
                } { 
                    note.asFloat 
                };
                var samplePaths = samples.collect { |sample|
                    if (basePath != "") { basePath ++ "/" ++ sample } { sample }
                };
                processedBank[noteMidi] = samplePaths;
            };
        },
        \autoDetect, {
            // Auto-detect MIDI notes from filenames (like superdough_sampler)
            var folder = config[\folder];
            var pattern = config[\pattern] ? "*";
            var files = (folder +/+ pattern).pathMatch;
            
            files.do { |file|
                var fileName = PathName(file).fileNameWithoutExtension;
                var noteMidi = fileName.findRegexp("[0-9]+").first !? { |match|
                    match[1].asInteger
                };
                if(noteMidi.notNil) {
                    if(processedBank[noteMidi].isNil) {
                        processedBank[noteMidi] = [];
                    };
                    processedBank[noteMidi] = processedBank[noteMidi].add(file);
                };
            };
        },
        \chromatic, {
            // Chromatic mapping starting from a root note
            var root = config[\root] ? 60; // C4
            var samples = config[\samples];
            
            samples.do { |sample, i|
                var noteMidi = root + i;
                var samplePath = if (basePath != "") { basePath ++ "/" ++ sample } { sample };
                processedBank[noteMidi] = [samplePath];
            };
        }
    );
    
    ~tidalPitchBanks[bankName] = processedBank;
    ("Registered Tidal pitch bank:" + bankName + "with" + processedBank.size + "notes using" + loadStrategy + "strategy").postln;
    
    // Pre-load buffers for better performance (optional)
    if(config[\preload] == true) {
        ~preloadTidalPitchBank.value(bankName);
    };
};

// Pre-load all buffers for a bank
~preloadTidalPitchBank = { |bankName|
    var bank = ~tidalPitchBanks[bankName];
    if(bank.notNil) {
        bank.keysValuesDo { |midi, samples|
            samples.do { |samplePath|
                var bufKey = (bankName.asString ++ "_" ++ midi.asString ++ "_" ++ samplePath.hash.asString).asSymbol;
                if(~tidalPitchBuffers[bufKey].isNil) {
                    ~tidalPitchBuffers[bufKey] = Buffer.read(s, samplePath);
                };
            };
        };
        ("Pre-loaded buffers for bank:" + bankName).postln;
    };
};

// Enhanced closest sample finder with SuperDough algorithm
~findTidalClosestSample = { |bankName, targetMidi, n = 0, speed = 1|
    var bank = ~tidalPitchBanks[bankName];
    var closestNote, minDiff = inf, transpose, samplePath, playbackRate;
    
    if (bank.isNil) {
        ("Tidal pitch bank not found:" + bankName).warn;
        ^nil;
    };
    
    // SuperDough algorithm: find closest by MIDI difference
    bank.keys.do { |noteMidi|
        var diff = abs(noteMidi - targetMidi);
        if (diff < minDiff) {
            minDiff = diff;
            closestNote = noteMidi;
        };
    };
    
    // Calculate transpose (negative because we're going FROM closest TO target)
    transpose = targetMidi - closestNote;
    
    // Get sample path with n cycling
    var samples = bank[closestNote];
    var index = n % samples.size;
    samplePath = samples[index];
    
    // Calculate playback rate: speed * 2^(transpose/12)
    playbackRate = speed.abs * (2.pow(transpose / 12.0));
    
    ^(
        samplePath: samplePath,
        transpose: transpose,
        playbackRate: playbackRate,
        reversedSpeed: speed < 0,
        closestMidi: closestNote,
        targetMidi: targetMidi
    );
};

// Comprehensive SynthDef with Tidal parameter compatibility
SynthDef(\tidalPitchSampler, {
    |out = 0, freq = 440, amp = 0.1, pan = 0, 
     // Tidal envelope params
     attack = 0.01, decay = 0.1, sustain = 0.8, release = 0.5,
     // Tidal sample params  
     begin = 0, end = 1, speed = 1, loop = 0,
     // Tidal filter params
     cutoff = 20000, resonance = 1, hcutoff = 10, hresonance = 1,
     // Tidal effects
     room = 0, size = 0.1, dry = 1, wet = 0,
     delay = 0, delaytime = 0.25, delayfeedback = 0.5,
     // Tidal modulation
     tremolorate = 0, tremolodepth = 0, vibratorate = 0, vibratodepth = 0,
     // SuperDirt compatibility
     orbit = 0, velocity = 1|
    
    var env, sig, filter, reverb, delayFX, tremolo, vibrato;
    var buf = \buf.kr(0);
    var rate = \rate.kr(1) * speed;
    var startPos = \startPos.kr(0);
    var dur = BufDur.kr(buf);
    var frames = BufFrames.kr(buf);
    var reversed = \reversed.kr(0);
    
    // Handle reversed playback
    if(reversed > 0) {
        rate = rate * -1;
        startPos = frames - 1;
    };
    
    // ADSR envelope compatible with Tidal
    env = EnvGen.kr(
        Env.adsr(attack, decay, sustain, release),
        gate: \gate.kr(1),
        doneAction: Done.freeSelf
    );
    
    // Sample playback with precise slicing
    sig = PlayBuf.ar(
        numChannels: 2,
        bufnum: buf,
        rate: BufRateScale.kr(buf) * rate,
        startPos: startPos + (begin * frames),
        loop: loop,
        doneAction: if(loop > 0, Done.none, Done.freeSelf)
    );
    
    // Apply end slicing if needed
    if (end < 1) {
        var sliceDur = (end - begin) * dur;
        var sliceEnv = EnvGen.kr(
            Env([1, 1, 0], [sliceDur, 0.01]),
            doneAction: Done.none
        );
        sig = sig * sliceEnv;
    };
    
    // Tidal-style filtering (low-pass and high-pass)
    if(cutoff < 20000) {
        sig = LPF.ar(sig, cutoff.lag(0.02), resonance);
    };
    if(hcutoff > 10) {
        sig = HPF.ar(sig, hcutoff.lag(0.02), hresonance);
    };
    
    // Modulation effects
    if(tremolorate > 0) {
        tremolo = SinOsc.kr(tremolorate, 0, tremolodepth, 1 - tremolodepth);
        sig = sig * tremolo;
    };
    
    if(vibratorate > 0) {
        vibrato = SinOsc.kr(vibratorate, 0, vibratodepth);
        rate = rate * (1 + vibrato);
    };
    
    // Apply envelope, velocity, and amplitude
    sig = sig * env * velocity * amp;
    
    // Panning
    sig = Pan2.ar(sig.sum, pan);
    
    // Built-in reverb (simple but effective)
    if(room > 0) {
        reverb = FreeVerb.ar(sig, room, size, 0.5);
        sig = (sig * dry) + (reverb * wet.max(room));
    };
    
    // Built-in delay
    if(delay > 0 && delaytime > 0) {
        delayFX = CombL.ar(sig, 2.0, delaytime, delayfeedback);
        sig = sig + (delayFX * delay);
    };
    
    Out.ar(out, sig);
}).add;

// SuperDirt integration function
~integrateTidalPitchSampler = {
    // Add the synth to SuperDirt's library
    ~dirt.soundLibrary.addSynth(\tidalPitch, (
        instrument: \tidalPitchSampler,
        bankName: \default,
        n: 0,
        speed: 1
    ));
    
    // Enhanced event type with full Tidal compatibility
    Event.addEventType(\tidalPitch, { |event|
        var bankName = event[\bankName] ?? event[\s] ?? \default;
        var targetMidi = if(event[\freq].notNil) { 
            event[\freq].cpsmidi 
        } { 
            if(event[\note].notNil) {
                event[\note].notemidi
            } {
                event[\midinote] ?? 60
            }
        };
        var n = event[\n] ?? 0;
        var speed = event[\speed] ?? 1;
        
        var sampleInfo = ~findTidalClosestSample.value(bankName, targetMidi, n, speed);
        var bufKey, buffer;
        
        if(sampleInfo.isNil) {
            ("No sample found for note" + targetMidi + "in bank" + bankName).warn;
            ^nil;
        };
        
        // Try to get cached buffer first
        bufKey = (bankName.asString ++ "_" ++ sampleInfo.closestMidi.asString ++ "_" ++ sampleInfo.samplePath.hash.asString).asSymbol;
        buffer = ~tidalPitchBuffers[bufKey];
        
        if(buffer.isNil or: { buffer.bufnum.isNil }) {
            // Load buffer on demand
            buffer = Buffer.read(s, sampleInfo.samplePath, action: { |buf|
                if(buf.notNil) {
                    ~tidalPitchBuffers[bufKey] = buf;
                    
                    // Play the synth
                    Synth(\tidalPitchSampler, [
                        \out, event[\out] ?? 0,
                        \buf, buf.bufnum,
                        \rate, sampleInfo.playbackRate,
                        \reversed, if(sampleInfo.reversedSpeed, 1, 0),
                        \amp, event[\amp] ?? 0.1,
                        \pan, event[\pan] ?? 0,
                        \attack, event[\attack] ?? 0.01,
                        \decay, event[\decay] ?? 0.1,
                        \sustain, event[\sustain] ?? 0.8,
                        \release, event[\release] ?? 0.5,
                        \begin, event[\begin] ?? 0,
                        \end, event[\end] ?? 1,
                        \speed, event[\speed] ?? 1,
                        \loop, event[\loop] ?? 0,
                        \cutoff, event[\cutoff] ?? 20000,
                        \resonance, event[\resonance] ?? 1,
                        \hcutoff, event[\hcutoff] ?? 10,
                        \hresonance, event[\hresonance] ?? 1,
                        \room, event[\room] ?? 0,
                        \size, event[\size] ?? 0.1,
                        \dry, event[\dry] ?? 1,
                        \wet, event[\wet] ?? 0,
                        \delay, event[\delay] ?? 0,
                        \delaytime, event[\delaytime] ?? 0.25,
                        \delayfeedback, event[\delayfeedback] ?? 0.5,
                        \tremolorate, event[\tremolorate] ?? 0,
                        \tremolodepth, event[\tremolodepth] ?? 0,
                        \vibratorate, event[\vibratorate] ?? 0,
                        \vibratodepth, event[\vibratodepth] ?? 0,
                        \orbit, event[\orbit] ?? 0,
                        \velocity, event[\velocity] ?? 1
                    ]);
                } {
                    ("Failed to load sample:" + sampleInfo.samplePath).error;
                };
            });
        } {
            // Buffer already loaded, play immediately
            Synth(\tidalPitchSampler, [
                \out, event[\out] ?? 0,
                \buf, buffer.bufnum,
                \rate, sampleInfo.playbackRate,
                \reversed, if(sampleInfo.reversedSpeed, 1, 0),
                \amp, event[\amp] ?? 0.1,
                \pan, event[\pan] ?? 0,
                \attack, event[\attack] ?? 0.01,
                \decay, event[\decay] ?? 0.1,
                \sustain, event[\sustain] ?? 0.8,
                \release, event[\release] ?? 0.5,
                \begin, event[\begin] ?? 0,
                \end, event[\end] ?? 1,
                \speed, event[\speed] ?? 1,
                \loop, event[\loop] ?? 0,
                \cutoff, event[\cutoff] ?? 20000,
                \resonance, event[\resonance] ?? 1,
                \hcutoff, event[\hcutoff] ?? 10,
                \hresonance, event[\hresonance] ?? 1,
                \room, event[\room] ?? 0,
                \size, event[\size] ?? 0.1,
                \dry, event[\dry] ?? 1,
                \wet, event[\wet] ?? 0,
                \delay, event[\delay] ?? 0,
                \delaytime, event[\delaytime] ?? 0.25,
                \delayfeedback, event[\delayfeedback] ?? 0.5,
                \tremolorate, event[\tremolorate] ?? 0,
                \tremolodepth, event[\tremolodepth] ?? 0,
                \vibratorate, event[\vibratorate] ?? 0,
                \vibratodepth, event[\vibratodepth] ?? 0,
                \orbit, event[\orbit] ?? 0,
                \velocity, event[\velocity] ?? 1
            ]);
        };
    });
    
    ("Tidal Pitch Sampler integrated with SuperDirt!").postln;
};

// Utility function to clean up unused buffers
~cleanupTidalPitchBuffers = {
    ~tidalPitchBuffers.keysValuesDo { |key, buffer|
        if(buffer.notNil) {
            buffer.free;
        };
    };
    ~tidalPitchBuffers.clear;
    ("Cleaned up Tidal pitch sampler buffers").postln;
};

// Example usage and configuration:

/*
// Register different types of banks:

// 1. Explicit note mapping (most precise)
~registerTidalPitchBank.value(\piano, (
    strategy: \noteMap,
    samples: (
        \c3: ["piano_c3_01.wav", "piano_c3_02.wav"],
        \d3: ["piano_d3_01.wav"],  
        \e3: ["piano_e3_01.wav", "piano_e3_02.wav"],
        \f3: ["piano_f3_01.wav"],
        \g3: ["piano_g3_01.wav"],
        \a3: ["piano_a3_01.wav", "piano_a3_02.wav"],
        \b3: ["piano_b3_01.wav"],
        \c4: ["piano_c4_01.wav", "piano_c4_02.wav"]
    ),
    preload: true
), "/path/to/piano/samples");

// 2. Auto-detect from filenames (compatible with existing sample libraries)
~registerTidalPitchBank.value(\violin, (
    strategy: \autoDetect,
    folder: "/path/to/violin/samples",
    pattern: "*.wav",
    preload: false
));

// 3. Chromatic mapping (for multi-sampled instruments)
~registerTidalPitchBank.value(\strings, (
    strategy: \chromatic,
    root: 48, // C3
    samples: [
        "string_c3.wav", "string_cs3.wav", "string_d3.wav", 
        "string_ds3.wav", "string_e3.wav", "string_f3.wav"
    ]
), "/path/to/string/samples");

// Integrate with SuperDirt (call after SuperDirt is loaded)
~integrateTidalPitchSampler.value;

// Tidal usage examples:
// d1 $ s "tidalPitch" # bankName "piano" # note "c4 d4 e4 f4"
// d1 $ s "tidalPitch" # bankName "piano" # note (scale "major" "0 1 2 3 4 5 6 7") # n "0 1"
// d1 $ s "tidalPitch" # bankName "violin" # note "c4*4" # cutoff (range 500 8000 $ slow 4 sine)
// d1 $ s "tidalPitch" # bankName "strings" # note "c3 e3 g3" # room 0.3 # size 0.8

*/

"Tidal-Integrated Pitch-Aware Sampler loaded successfully!".postln;
)