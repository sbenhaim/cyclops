
(
s.waitForBoot {
    ~dirt.loadSoundFiles;
    s.sync;

    SynthDef("tidal_pitch_sampler", { |out, freq = 440, sustain = 1, pan = 0.5, begin = 0, end = 1, speed = 1, sample, midinote, attack=0.01, release=0.5, cutoff=20000, resonance=1, room=0, size=0.1|
        var sig, env, bufnum, rate;
        bufnum = BufDef.find(sample, midinote);
        rate = (freq / midinote.midicps) * speed;
        sig = PlayBuf.ar(
            numChannels: 2,
            bufnum: bufnum,
            rate: BufRateScale.kr(bufnum) * rate,
            startPos: begin * BufFrames.kr(bufnum),
            loop: 0
        );
        env = EnvGen.ar(Env.linen(attack, sustain, release), doneAction: 2);
        sig = sig * env;
        sig = LPF.ar(sig, cutoff, resonance);
        if (room > 0) {
          sig = sig + (FreeVerb.ar(sig, room, size, 0.5) * room);
        };
        sig = Pan2.ar(sig, pan);
        Out.ar(out, sig);
    }).add;

    s.sync;

    ~dirt.soundLibrary.add(
        'psample',
        (
            soundInfo: (
                folder: "psample",
                vars: (
                    n: 0
                )
            ),
            synth: "tidal_pitch_sampler",
            play: { |event|
                var sampleName = event.s.asString;
                var note = event.n.asFloat;
                var midinote = note.round(1);
                var soundFiles = ~dirt.soundLibrary.at(sampleName).soundInfo.files;
                var closest, closestMidi;

                // Find the closest sample
                closest = soundFiles.reduce({|a, b|
                    var aMidi = a.fileName.split($. ).first.asInteger;
                    var bMidi = b.fileName.split($. ).first.asInteger;
                    if((aMidi - midinote).abs < (bMidi - midinote).abs, { a }, { b })
                });

                closestMidi = closest.fileName.split($. ).first.asInteger;

                event.put(\sample, closest.path.asString);
                event.put(\midinote, closestMidi);

                Synth.new("tidal_pitch_sampler", [
                    out: event.out,
                    freq: note.midicps,
                    sustain: event.sustain,
                    pan: event.pan,
                    begin: event.begin,
                    end: event.end,
                    speed: event.speed,
                    sample: event.sample,
                    midinote: event.midinote,
                    attack: event.get('attack', 0.01),
                    release: event.get('release', 0.5),
                    cutoff: event.get('cutoff', 20000),
                    resonance: event.get('resonance', 1),
                    room: event.get('room', 0),
                    size: event.get('size', 0.1)
                ]);
            }
        )
    );

    "Tidal Pitch Sampler loaded. Use with s \"psample\"".postln;
}
)
