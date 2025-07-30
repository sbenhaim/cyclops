// SuperDirt Pitch-Aware Sampler
// Based on SuperDough implementation from Strudel

// Dictionary to store sample banks with pitch information
~pitchSampleBanks = Dictionary.new;

// Function to register a pitch-aware sample bank
~registerPitchBank = { |bankName, sampleDict, basePath = ""|
    var processedBank = Dictionary.new;
    
    sampleDict.keysValuesDo { |note, samples|
        var noteMidi = note.asString.notemidi;
        var samplePaths = samples.collect { |sample|
            if (basePath != "") {
                basePath ++ "/" ++ sample
            } {
                sample
            }
        };
        processedBank[noteMidi] = samplePaths;
    };
    
    ~pitchSampleBanks[bankName] = processedBank;
    ("Registered pitch bank:" + bankName + "with" + processedBank.size + "notes").postln;
};

// Function to find closest sample and calculate transpose amount
~findClosestSample = { |bankName, targetMidi, n = 0|
    var bank = ~pitchSampleBanks[bankName];
    var closestNote, minDiff = inf, transpose, samplePath;
    
    if (bank.isNil) {
        ("Bank not found:" + bankName).warn;
        ^nil;
    };
    
    // Find closest MIDI note in bank
    bank.keys.do { |noteMidi|
        var diff = abs(noteMidi - targetMidi);
        if (diff < minDiff) {
            minDiff = diff;
            closestNote = noteMidi;
        };
    };
    
    // Calculate transpose in semitones
    transpose = closestNote - targetMidi;
    
    // Get sample path (cycle through available samples with n)
    var samples = bank[closestNote];
    var index = n % samples.size;
    samplePath = samples[index];
    
    ^(
        samplePath: samplePath,
        transpose: transpose,
        playbackRate: 2.pow(transpose / 12.0)
    );
};

// Enhanced SynthDef for pitch-aware sampling
SynthDef(\pitchSampler, {
    |out = 0, freq = 440, amp = 0.1, pan = 0, attack = 0.01, decay = 0.1, 
     sustain = 0.8, release = 0.5, begin = 0, end = 1, speed = 1, 
     cutoff = 20000, resonance = 1, room = 0, size = 0.1|
    
    var env, sig, filter;
    var buf = \buf.kr(0);
    var rate = \rate.kr(1) * speed;
    var startPos = \startPos.kr(0);
    var dur = BufDur.kr(buf);
    
    // ADSR envelope
    env = EnvGen.kr(
        Env.adsr(attack, decay, sustain, release),
        gate: \gate.kr(1),
        doneAction: Done.freeSelf
    );
    
    // Sample playback with pitch adjustment
    sig = PlayBuf.ar(
        numChannels: 2,
        bufnum: buf,
        rate: BufRateScale.kr(buf) * rate,
        startPos: startPos,
        loop: 0,
        doneAction: Done.freeSelf
    );
    
    // Apply slice (begin/end)
    if (begin != 0 or: { end != 1 }) {
        var sliceStart = begin * dur * SampleRate.ir;
        var sliceEnd = end * dur * SampleRate.ir;
        sig = sig * EnvGen.kr(
            Env([0, 1, 1, 0], [0, sliceEnd - sliceStart, 0] / SampleRate.ir),
            timeScale: 1,
            doneAction: Done.none
        );
    };
    
    // Low-pass filter
    filter = LPF.ar(sig, cutoff, resonance);
    
    // Apply envelope and panning
    sig = filter * env * amp;
    sig = Pan2.ar(sig.sum, pan);
    
    // Optional reverb send
    if (room > 0) {
        sig = sig + (FreeVerb.ar(sig, room, size, 0.5) * room);
    };
    
    Out.ar(out, sig);
}).add;

// Function to play pitch-aware samples
~playPitchSample = { |bankName, note = \c4, n = 0, dur = 1, speed = 1, amp = 0.1, 
                    attack = 0.01, decay = 0.1, sustain = 0.8, release = 0.5,
                    begin = 0, end = 1, cutoff = 20000, resonance = 1, 
                    room = 0, size = 0.1, pan = 0|
    
    var targetMidi = note.asFloat ?? note.notemidi;
    var sampleInfo = ~findClosestSample.value(bankName, targetMidi, n);
    var buf, synth;
    
    if (sampleInfo.isNil) {
        ("No sample found for note" + note + "in bank" + bankName).warn;
        ^nil;
    };
    
    // Load buffer if not already loaded
    buf = Buffer.read(s, sampleInfo.samplePath, action: { |buffer|
        if (buffer.notNil) {
            synth = Synth(\pitchSampler, [
                \buf, buffer.bufnum,
                \rate, sampleInfo.playbackRate,
                \amp, amp,
                \attack, attack,
                \decay, decay,
                \sustain, sustain,
                \release, release,
                \begin, begin,
                \end, end,
                \speed, speed,
                \cutoff, cutoff,
                \resonance, resonance,
                \room, room,
                \size, size,
                \pan, pan
            ]);
            
            // Schedule buffer cleanup
            SystemClock.sched(dur + release + 1, {
                buffer.free;
                nil;
            });
        } {
            ("Failed to load sample:" + sampleInfo.samplePath).error;
        };
    });
    
    ^synth;
};

// Example usage:

// Register a sample bank with pitch information
~registerPitchBank.value(\piano, (
    \c3: ["piano_c3_01.wav", "piano_c3_02.wav"],
    \d3: ["piano_d3_01.wav"],  
    \e3: ["piano_e3_01.wav", "piano_e3_02.wav"],
    \f3: ["piano_f3_01.wav"],
    \g3: ["piano_g3_01.wav"],
    \a3: ["piano_a3_01.wav", "piano_a3_02.wav"],
    \b3: ["piano_b3_01.wav"],
    \c4: ["piano_c4_01.wav", "piano_c4_02.wav"]
), "/path/to/piano/samples");

/*
// Usage examples:

// Play C4 (will use exact sample)
~playPitchSample.value(\piano, \c4, 0, 1, 1, 0.5);

// Play C#4 (will use closest sample and transpose)  
~playPitchSample.value(\piano, \cs4, 0, 1, 1, 0.5);

// Play with different sample variation (n parameter)
~playPitchSample.value(\piano, \c3, 1, 1, 1, 0.5);

// Play with effects
~playPitchSample.value(\piano, \a3, 0, 2, 1, 0.3, 
    cutoff: 8000, room: 0.3, size: 0.8);
*/

// Integration function for SuperDirt
~addPitchSamplerToDirt = {
    ~dirt.soundLibrary.addSynth(\pitchSample, (
        instrument: \pitchSampler,
        bankName: \default,
        n: 0
    ));
    
    // Custom event type for pitch sampling
    Event.addEventType(\pitchSample, { |server|
        var bankName = ~bankName ?? \default;
        var note = ~freq.cpsmidi ?? ~note.notemidi ?? 60;
        var n = ~n ?? 0;
        
        ~playPitchSample.value(
            bankName, note, n, ~dur, ~speed ?? 1, ~amp ?? 0.1,
            ~attack ?? 0.01, ~decay ?? 0.1, ~sustain ?? 0.8, ~release ?? 0.5,
            ~begin ?? 0, ~end ?? 1, ~cutoff ?? 20000, ~resonance ?? 1,
            ~room ?? 0, ~size ?? 0.1, ~pan ?? 0
        );
    });
};

// Call this after SuperDirt is loaded
// ~addPitchSamplerToDirt.value;

"SuperDirt Pitch-Aware Sampler loaded!".postln;