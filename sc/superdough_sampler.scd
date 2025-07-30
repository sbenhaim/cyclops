
(
s.waitForBoot {
    ~dirt.loadSoundFiles;
    s.sync;

    SynthDef(\superdough_sampler, { |out, freq = 440, sustain = 1, pan = 0.5, begin = 0, end = 1, speed = 1, sample, note, midinote|
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
        env = EnvGen.ar(Env.perc(0.01, sustain), doneAction: 2);
        sig = sig * env;
        sig = Pan2.ar(sig, pan);
        Out.ar(out, sig);
    }).add;

    s.sync;

    ~dirt.soundLibrary.add(
        'superdough',
        (
            soundInfo: (
                folder: "superdough",
                vars: (
                    n: 0
                )
            ),
            synth: \superdough_sampler,
            play: { |event|
                var sampleName = event.s.asString;
                var note = event.n.asFloat;
                var midinote = note.round(1);
                var sampleMap = ~dirt.soundLibrary.at(sampleName).soundInfo.folder;
                var soundFiles = ~dirt.soundLibrary.at(sampleName).soundInfo.files;
                var closest, midiDiff, closestMidi;

                // Find the closest sample
                closest = soundFiles.reduce({|a, b|
                    var aMidi = a.fileName.split($.).first.asInteger;
                    var bMidi = b.fileName.split($.).first.asInteger;
                    if((aMidi - midinote).abs < (bMidi - midinote).abs, { a }, { b })
                });

                closestMidi = closest.fileName.split($.).first.asInteger;

                event.put(\sample, closest.path.asString);
                event.put(\midinote, closestMidi);

                Synth.new(\superdough_sampler, [
                    out: event.out,
                    freq: note.midicps,
                    sustain: event.sustain,
                    pan: event.pan,
                    begin: event.begin,
                    end: event.end,
                    speed: event.speed,
                    sample: event.sample,
                    midinote: event.midinote
                ]);
            }
        )
    );
}
)
