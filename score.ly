\version "2.18.2"
\include "articulate.ly"
#(set-global-staff-size 16)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  http://lsr.di.unimi.it/LSR/Item?id=445

%LSR by Jay Anderson.
%modyfied by Simon Albrecht on March 2014.
%=> http://lilypond.1069038.n5.nabble.com/LSR-445-error-td160662.html

#(define (octave-up m t)
 (let* ((octave (1- t))
      (new-note (ly:music-deep-copy m))
      (new-pitch (ly:make-pitch
        octave
        (ly:pitch-notename (ly:music-property m 'pitch))
        (ly:pitch-alteration (ly:music-property m 'pitch)))))
  (set! (ly:music-property new-note 'pitch) new-pitch)
  new-note))

#(define (octavize-chord elements t)
 (cond ((null? elements) elements)
     ((eq? (ly:music-property (car elements) 'name) 'NoteEvent)
       (cons (car elements)
             (cons (octave-up (car elements) t)
                   (octavize-chord (cdr elements) t))))
     (else (cons (car elements) (octavize-chord (cdr elements ) t)))))

#(define (octavize music t)
 (if (eq? (ly:music-property music 'name) 'EventChord)
       (ly:music-set-property! music 'elements (octavize-chord
(ly:music-property music 'elements) t)))
 music)

makeOctaves = #(define-music-function (parser location arg mus) (integer? ly:music?)
 (music-map (lambda (x) (octavize x arg)) (event-chord-wrap! mus)))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cr = \change Staff = "right"
cl = \change Staff = "left"
rhMark = \markup { 
  \path #0.1 #'((moveto -1 0)(rlineto 0 -1.5)(rlineto 0.5 0))
}

\header {
  title = "春田花花幼稚園校歌"
  subtitle = "For soloist and piano accompaniment"
  arranger = "Arranged by Benson"
}

upper-prelude = \relative c' {
  <d d'>8\( q <e g e'>4. <f a f'>8 <e g e'>4\) <g g'>8\( q <a c a'>4. <b d b'>8 <a c a'>4\)
  <g g'>8\( q <a c a'>4 <a a'>8 q <b b'> <a a'> <b b'> <g g'> <c e g c>2.\)
}

lower-prelude = \relative c, {
  r4 \makeOctaves 1 { c4.\( d8 c4\) e4\( f4. g8 f4\) e4 f d g g }
  c4-- g-- c,--
}

upper-verse = \relative c' {
  r4
  <e g c>4 <c e> <e g c> q
  <f a c>4 <c f> <e g c> q
  <c f a>4 q <c e g> q
  <c f>4 q <d g> q
  <c e g>4 <c e>8 <c f> <c e g>4 q
  <c f a>4 <c f>8 <d g> <c f a>4 <c e g>
  <c f a>4 q <b d g> <b d>
  <c e g>2.
}

lower-verse = \relative c, {
  r4
  \makeOctaves 1 {
    c4 c e e f f e e f f e e f f g g
    c,4 } c8 d \makeOctaves 1 { c4 e f f f e d d g g
  }
  c4-- g-- c,--
}

pedals = {
  s8 s4
  % s2\sustainOn
  % \repeat unfold 22 { s2\sustainOff\sustainOn }
}

upper-c-major = \relative c' {
  \clef treble
  \tempo "Maestoso" 4 = 98
  \time 4/4
  \key c \major
  \partial 4
  \upper-prelude
  \upper-verse
  \upper-verse
  r4
  \bar "|."
}

lower-c-major = \relative c {
  \clef bass
  \time 4/4
  \key c \major
  \partial 4
  \lower-prelude
  \lower-verse
  \lower-verse
  r4
  \bar "|."
}

dynamics = {
  s4\f s1*4
  s1\mf
  s1*14
  s1_"rit."
}

guitarchords = \chordmode {
  % ais1:m
}

lyricsmain = \lyricmode {
  鵝 悶 是 快 烙 滴 好 耳 痛 鵝 悶 天 天 一 起 個 窗
  鵝 悶 在 殼 習 鵝 悶 載 升 賬 鵝 悶 是 春 天 滴 化

  鵝 滿 是 咪 耐 滴 主 印 勇 鵝 悶 屎 舍 燴 滴 洞 亮
  姣 象 你 耗 罵 路 時 你 好 馬 鵝 悶 田 田 問 喉 您
}

melody-c-major = \relative c'' {
  \clef treble
  \time 4/4
  \key c \major
  \partial 4
  r4 R1*3 r2 r4
  g4\( c4. b8 d c g( e) a4. g8 g4\)
  g4\( a a8 a g e4 c8 d2\) r4
  d8\( d e4. f8 e4\) g8\( g a4. b8 a4\)
  g8\( g a4 a8 a b( a4 g8) c2\) r4
  g4\( c4. b8 d c g( e) a4. g8 g4\)
  g4\( a a8 a g e4 c8 d2\) r4
  d8\( d e4. f8 e4\) g8\( g a4. b8 a4\)
  g8\( g a4 a4 b8( a4) g8 c2\) r2
  \bar "|."
}

\book {
\bookOutputSuffix "c-major"
\score {
  <<
    % \new ChordNames {
      % \set chordChanges = ##t
      % \guitarchords
    % }
    \new Staff = "melodystaff" \with {
      % fontSize = #-3
      % \override StaffSymbol.staff-space = #(magstep -3)
      % \override StaffSymbol.thickness = #(magstep -3)
    }
    <<
      \set Staff.instrumentName = #"Solo"
      \new Voice = "melody" {
        \melody-c-major
      }
      \context Lyrics = "lyrics" { \lyricsto "melody" { \lyricsmain } }
    >>
    \new PianoStaff <<
      \set PianoStaff.instrumentName = #"Piano"
      \new Staff = "right" { \upper-c-major }
      \new Dynamics = "Dynamics_pf" \dynamics
      \new Staff = "left" { \lower-c-major }
    >>
    \new Staff <<
      % TODO: pedal sustain style bracket not working
      \set Staff.pedalSustainStyle = #'bracket

      \new Dynamics = "pedal-marking" \pedals
    >>
  >>
  \layout {
    \context {
      % add the RemoveEmptyStaffContext that erases rest-only staves
      \Staff \RemoveEmptyStaves
    }
    \context {
      % add the RemoveEmptyStaffContext that erases rest-only staves
      \Dynamics \RemoveEmptyStaves
    }
    \context {
      \Score
      % Remove all-rest staves also in the first system
      \override VerticalAxisGroup.remove-first = ##t
      % If only one non-empty staff in a system exists, still print the starting bar
      \override SystemStartBar.collapse-height = #1
    }
    \context {
      \ChordNames
      \override ChordName #'font-size = #-3
    }
  }
}
\score {
  <<
    % \new ChordNames {
      % \guitarchords
    % }
    \new Staff = "melodystaff" <<
      % \set Staff.midiInstrument = #"electric guitar (clean)"
      \set Staff.midiInstrument = #"recorder"
      \set Staff.instrumentName = #"Ocarina"
      \set Staff.midiMinimumVolume = #0.9
      \set Staff.midiMaximumVolume = #1
      \new Voice = "melody" {
        \articulate << \melody-c-major >>
      }
      \context Lyrics = "lyrics" { \lyricsto "melody" { \lyricsmain } }
    >>
    \new PianoStaff <<
      \new Staff = "right" {
        \set Staff.midiInstrument = #"acoustic grand"
        \set Staff.midiMinimumVolume = #0.5
        \set Staff.midiMaximumVolume = #0.6
        \articulate << \upper-c-major \pedals >>
      }
      \new Staff = "left" {
        \set Staff.midiInstrument = #"acoustic grand"
        \set Staff.midiMinimumVolume = #0.5
        \set Staff.midiMaximumVolume = #0.6
        \articulate << \lower-c-major \pedals >>
      }
    >>
  >>
  \midi {
    \context {
      \ChordNameVoice \remove Note_performer
    }
  }
}
}
