unit Actor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, uGameEngine,
  Grobal2, PXL.Canvas, PXL.Textures, AsphyreTextureFonts, cliUtil, magiceff, WIL, ClFunc, GList;

const
  MAXACTORSOUND             = 3;
  CMMX                      = 150;
  CMMY                      = 200;

  HUMANFRAME                = 600;
  MONFRAME                  = 280;
  EXPMONFRAME               = 360;
  SCULMONFRAME              = 440;
  ZOMBIFRAME                = 430;
  MERCHANTFRAME             = 60;
  MAXSAY                    = 5;

  RUN_MINHEALTH             = 10;
  DEFSPELLFRAME             = 10;
  FIREHIT_READYFRAME        = 6;
  MAGBUBBLEBASE             = 3890;     //ƒß∑®∂‹–ßπ˚ÕºŒª÷√
  MAGBUBBLESTRUCKBASE       = 3900;     //±ªπ•ª˜ ±ƒß∑®∂‹–ßπ˚ÕºŒª÷√
  MAXWPEFFECTFRAME          = 5;
  WPEFFECTBASE              = 3750;
  EffectBase                = 0;

type
  TActionInfo = packed record
    start: Word;
    frame: Word;
    skip: Word;
    ftime: Word;
    usetick: Word;
  end;
  pTActionInfo = ^TActionInfo;

  TMonsterAction = packed record
    ActStand: TActionInfo;
    ActWalk: TActionInfo;
    ActAttack: TActionInfo;
    ActCritical: TActionInfo;
    ActStruck: TActionInfo;
    ActDie: TActionInfo;
    ActDeath: TActionInfo;
  end;
  pTMonsterAction = ^TMonsterAction;

const
  MA9                       : TMonsterAction = (//4C03D4
    ActStand: (start: 0; frame: 1; skip: 7; ftime: 200; usetick: 0);
    ActWalk: (start: 64; frame: 6; skip: 2; ftime: 120; usetick: 3);
    ActAttack: (start: 64; frame: 6; skip: 2; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 64; frame: 6; skip: 2; ftime: 100; usetick: 0);
    ActDie: (start: 0; frame: 1; skip: 7; ftime: 140; usetick: 0);
    ActDeath: (start: 0; frame: 1; skip: 7; ftime: 0; usetick: 0);
    );
  MA10                      : TMonsterAction = (//(8Frame) ¥¯µ∂Œ¿ ø
    ActStand: (start: 0; frame: 4; skip: 4; ftime: 200; usetick: 0);
    ActWalk: (start: 64; frame: 6; skip: 2; ftime: 120; usetick: 3);
    ActAttack: (start: 128; frame: 4; skip: 4; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 192; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 208; frame: 4; skip: 4; ftime: 140; usetick: 0);
    ActDeath: (start: 272; frame: 1; skip: 0; ftime: 0; usetick: 0);
    );
  MA11                      : TMonsterAction = (//ªÁΩø(10Frame¬•∏Æ)
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 120; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 140; usetick: 0);
    ActDeath: (start: 340; frame: 1; skip: 0; ftime: 0; usetick: 0);
    );
  MA12                      : TMonsterAction = (//∞Ê∫Ò∫¥, ∂ß∏Æ¥¬ º”µµ ∫¸∏£¥Ÿ.
    ActStand: (start: 0; frame: 4; skip: 4; ftime: 200; usetick: 0);
    ActWalk: (start: 64; frame: 6; skip: 2; ftime: 120; usetick: 3);
    ActAttack: (start: 128; frame: 6; skip: 2; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 192; frame: 2; skip: 0; ftime: 150; usetick: 0);
    ActDie: (start: 208; frame: 4; skip: 4; ftime: 160; usetick: 0);
    ActDeath: (start: 272; frame: 1; skip: 0; ftime: 0; usetick: 0);
    );
  MA13                      : TMonsterAction = (//Ωƒ¿Œ√ 
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 10; frame: 8; skip: 2; ftime: 160; usetick: 0); //µÓ¿Â...
    ActAttack: (start: 30; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 110; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 130; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 20; frame: 9; skip: 0; ftime: 150; usetick: 0); //º˚¿Ω..
    );
  MA14                      : TMonsterAction = (//«ÿ∞Ò ø¿∏∂
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 340; frame: 10; skip: 0; ftime: 100; usetick: 0); //πÈ∞Ò¿Œ∞ÊøÏ(º“»Ø)
    );
  MA15                      : TMonsterAction = (//µµ≥¢¥¯¡ˆ¥¬ ø¿∏∂
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 1; frame: 1; skip: 0; ftime: 100; usetick: 0);
    );
  MA16                      : TMonsterAction = (//∞°Ω∫ΩÓ¥¬ ±∏µ•±‚
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 4; skip: 6; ftime: 160; usetick: 0);
    ActDeath: (start: 0; frame: 1; skip: 0; ftime: 160; usetick: 0);
    );
  MA17                      : TMonsterAction = (//πŸµ¸≤®∏Æ¥¬ ∏˜
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 60; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 100; usetick: 0);
    ActDeath: (start: 340; frame: 1; skip: 0; ftime: 140; usetick: 0); //
    );
  MA19                      : TMonsterAction = (//øÏ∏È±Õ (¡◊¥¬∞≈ ª°∏Æ¡◊¿Ω)
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 140; usetick: 0);
    ActDeath: (start: 340; frame: 1; skip: 0; ftime: 140; usetick: 0); //
    );
  MA20                      : TMonsterAction = (//¡◊æ˙¥Ÿ ªÏæ∆≥™¥¬ ¡ª∫Ò)
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 100; usetick: 0);
    ActDeath: (start: 340; frame: 10; skip: 0; ftime: 170; usetick: 0); //¥ŸΩ√ ªÏæ∆≥™±‚
    );
  MA21                      : TMonsterAction = (//π˙¡˝
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0); //
    ActAttack: (start: 10; frame: 6; skip: 4; ftime: 120; usetick: 0); //π˙ πﬂªÁ
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 20; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 30; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0); //
    );
  MA22                      : TMonsterAction = (//ºÆªÛ∏ÛΩ∫≈Õ(ø∞º“¥Î¿Â,ø∞º“¿Â±∫)
    ActStand: (start: 80; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 240; frame: 6; skip: 4; ftime: 100; usetick: 0); //
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 320; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 340; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActDeath: (start: 0; frame: 6; skip: 4; ftime: 170; usetick: 0); //ºÆªÛ≥Ï¿Ω
    );
  MA23                      : TMonsterAction = (//¡÷∏∂ø’
    ActStand: (start: 20; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 100; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 180; frame: 6; skip: 4; ftime: 100; usetick: 0); //
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 260; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 280; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActDeath: (start: 0; frame: 20; skip: 0; ftime: 100; usetick: 0); //ºÆªÛ≥Ï¿Ω
    );
  MA24                      : TMonsterAction = (//¿¸∞•, ∞¯∞› 2∞°¡ˆ
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 240; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActStruck: (start: 320; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 340; frame: 10; skip: 0; ftime: 140; usetick: 0);
    ActDeath: (start: 420; frame: 1; skip: 0; ftime: 140; usetick: 0); //
    );
  MA25                      : TMonsterAction = (//4C080C
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 70; frame: 10; skip: 0; ftime: 200; usetick: 3);
    ActAttack: (start: 20; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 10; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 50; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 60; frame: 10; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 80; frame: 10; skip: 0; ftime: 200; usetick: 3);
    );

  MA26                      : TMonsterAction = (//º∫πÆ,
    ActStand: (start: 0; frame: 1; skip: 7; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 160; usetick: 0); //µÓ¿Â...
    ActAttack: (start: 56; frame: 6; skip: 2; ftime: 350; usetick: 0); //ø≠±‚
    ActCritical: (start: 64; frame: 6; skip: 2; ftime: 350; usetick: 0); //¥›±‚
    ActStruck: (start: 0; frame: 4; skip: 4; ftime: 100; usetick: 0);
    ActDie: (start: 24; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0); //º˚¿Ω..
    );
  MA27                      : TMonsterAction = (//º∫∫Æ
    ActStand: (start: 0; frame: 1; skip: 7; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 160; usetick: 0); //µÓ¿Â...
    ActAttack: (start: 0; frame: 0; skip: 0; ftime: 250; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 250; usetick: 0);
    ActStruck: (start: 0; frame: 0; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 0; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0); //º˚¿Ω..
    );
  MA28                      : TMonsterAction = (//Ω≈ºˆ (∫ØΩ≈ ¿¸)
    ActStand: (start: 80; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 0; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 0; frame: 10; skip: 0; ftime: 100; usetick: 0); //µÓ¿Â..
    );
  MA29                      : TMonsterAction = (//Ω≈ºˆ (∫ØΩ≈ »ƒ)
    ActStand: (start: 80; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 3); //
    ActAttack: (start: 240; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 0; frame: 10; skip: 0; ftime: 100; usetick: 0);
    ActStruck: (start: 320; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 340; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 0; frame: 10; skip: 0; ftime: 100; usetick: 0); //µÓ¿Â..
    );
  MA30                      : TMonsterAction = (//4C0974
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 10; skip: 0; ftime: 160; usetick: 3);
    ActAttack: (start: 10; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 10; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActStruck: (start: 20; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 30; frame: 20; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 0; frame: 10; skip: 0; ftime: 140; usetick: 3);
    );
  MA31                      : TMonsterAction = (//4C09BC
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 10; skip: 0; ftime: 200; usetick: 3);
    ActAttack: (start: 10; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 0; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 0; frame: 2; skip: 8; ftime: 100; usetick: 0);
    ActDie: (start: 20; frame: 10; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 0; frame: 10; skip: 0; ftime: 200; usetick: 3);
    );

  MA32                      : TMonsterAction = (
    ActStand: (start: 0; frame: 1; skip: 9; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 6; skip: 4; ftime: 200; usetick: 3);
    ActAttack: (start: 0; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 0; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 0; frame: 2; skip: 8; ftime: 100; usetick: 0);
    ActDie: (start: 80; frame: 10; skip: 0; ftime: 80; usetick: 0);
    ActDeath: (start: 80; frame: 10; skip: 0; ftime: 200; usetick: 3);
    );

  MA33                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 200; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 340; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 260; frame: 10; skip: 0; ftime: 200; usetick: 0);
    );

  MA34                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 200; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 320; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 400; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 420; frame: 20; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 420; frame: 20; skip: 0; ftime: 200; usetick: 0);
    );

  MA35                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 30; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 0; frame: 1; skip: 9; ftime: 0; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA111                     : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 30; frame: 23; skip: 0; ftime: 180; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 0; frame: 1; skip: 9; ftime: 0; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA36                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 30; frame: 20; skip: 0; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 0; frame: 1; skip: 9; ftime: 0; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA37                      : TMonsterAction = (
    ActStand: (start: 30; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 30; frame: 4; skip: 6; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 0; frame: 1; skip: 9; ftime: 0; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA38                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 80; frame: 6; skip: 4; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA39                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 300; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 10; frame: 6; skip: 4; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 20; frame: 2; skip: 0; ftime: 150; usetick: 0);
    ActDie: (start: 30; frame: 10; skip: 0; ftime: 80; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA40                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 250; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 210; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 110; usetick: 0);
    ActCritical: (start: 580; frame: 20; skip: 0; ftime: 135; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 120; usetick: 0);
    ActDie: (start: 260; frame: 20; skip: 0; ftime: 130; usetick: 0);
    ActDeath: (start: 260; frame: 20; skip: 0; ftime: 130; usetick: 0);
    );

  MA41                      : TMonsterAction = (
    ActStand: (start: 0; frame: 2; skip: 8; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 2; skip: 8; ftime: 200; usetick: 0);
    ActAttack: (start: 0; frame: 2; skip: 8; ftime: 200; usetick: 0);
    ActCritical: (start: 0; frame: 2; skip: 8; ftime: 200; usetick: 0);
    ActStruck: (start: 0; frame: 2; skip: 8; ftime: 200; usetick: 0);
    ActDie: (start: 0; frame: 2; skip: 8; ftime: 200; usetick: 0);
    ActDeath: (start: 0; frame: 2; skip: 8; ftime: 200; usetick: 0);
    );

  MA42                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 10; frame: 8; skip: 2; ftime: 160; usetick: 0);
    ActAttack: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDie: (start: 30; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 30; frame: 10; skip: 0; ftime: 150; usetick: 0);
    );

  MA43                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActCritical: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 150; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 340; frame: 10; skip: 0; ftime: 100; usetick: 0);
    );

  MA44                      : TMonsterAction = (
    ActStand: (start: 0; frame: 10; skip: 0; ftime: 300; usetick: 0);
    ActWalk: (start: 10; frame: 6; skip: 4; ftime: 150; usetick: 0);
    ActAttack: (start: 20; frame: 6; skip: 4; ftime: 150; usetick: 0);
    ActCritical: (start: 40; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActStruck: (start: 40; frame: 2; skip: 8; ftime: 150; usetick: 0);
    ActDie: (start: 30; frame: 6; skip: 4; ftime: 150; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA45                      : TMonsterAction = (
    ActStand: (start: 0; frame: 10; skip: 0; ftime: 300; usetick: 0);
    ActWalk: (start: 0; frame: 10; skip: 0; ftime: 300; usetick: 0);
    ActAttack: (start: 10; frame: 10; skip: 0; ftime: 300; usetick: 0);
    ActCritical: (start: 10; frame: 10; skip: 0; ftime: 100; usetick: 0);
    ActStruck: (start: 0; frame: 1; skip: 9; ftime: 300; usetick: 0);
    ActDie: (start: 0; frame: 1; skip: 9; ftime: 300; usetick: 0);
    ActDeath: (start: 0; frame: 1; skip: 9; ftime: 300; usetick: 0);
    );

  MA46                      : TMonsterAction = (
    ActStand: (start: 0; frame: 20; skip: 0; ftime: 100; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );
  MA47                      : TMonsterAction = (
    ActStand: (start: 0; frame: 0; skip: 0; ftime: 200; usetick: 0);
    ActWalk: (start: 50; frame: 10; skip: 0; ftime: 200; usetick: 3);
    ActAttack: (start: 10; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 10; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 40; frame: 10; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 0; frame: 1; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 0; frame: 1; skip: 0; ftime: 200; usetick: 0);
    );
  MA48                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActCritical: (start: 340; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActDeath: (start: 0; frame: 1; skip: 0; ftime: 160; usetick: 0);
    );
  MA49                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActCritical: (start: 340; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActDeath: (start: 420; frame: 4; skip: 6; ftime: 200; usetick: 0);
    );

  MA50                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActCritical: (start: 340; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActDeath: (start: 420; frame: 4; skip: 6; ftime: 200; usetick: 0);
    );
  MA51                      : TMonsterAction = (
    ActStand: (start: 0; frame: 20; skip: 0; ftime: 150; usetick: 0);
    ActWalk: (start: 0; frame: 20; skip: 0; ftime: 150; usetick: 3);
    ActAttack: (start: 20; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActCritical: (start: 20; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActStruck: (start: 20; frame: 2; skip: 8; ftime: 100; usetick: 0);
    ActDie: (start: 400; frame: 18; skip: 0; ftime: 150; usetick: 0);
    ActDeath: (start: 400; frame: 18; skip: 0; ftime: 150; usetick: 0);
    );

  MA131                     : TMonsterAction = (
    ActStand: (start: 0; frame: 20; skip: 0; ftime: 150; usetick: 0);
    ActWalk: (start: 0; frame: 20; skip: 0; ftime: 150; usetick: 3);
    ActAttack: (start: 20; frame: 20; skip: 0; ftime: 150; usetick: 0);
    ActCritical: (start: 20; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActStruck: (start: 20; frame: 2; skip: 8; ftime: 100; usetick: 0);
    ActDie: (start: 400; frame: 18; skip: 0; ftime: 150; usetick: 0);
    ActDeath: (start: 400; frame: 18; skip: 0; ftime: 150; usetick: 0);
    );

  MA52                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 0);
    ActWalk: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 3);
    ActAttack: (start: 10; frame: 4; skip: 6; ftime: 300; usetick: 0);
    ActCritical: (start: 10; frame: 4; skip: 6; ftime: 300; usetick: 0);
    ActStruck: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 0);
    ActDie: (start: 0; frame: 4; skip: 6; ftime: 300; usetick: 0);
    ActDeath: (start: 0; frame: 4; skip: 6; ftime: 300; usetick: 0);
    );

  MA53                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 0);
    ActWalk: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 3);
    ActAttack: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 0);
    ActStruck: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 0);
    ActDie: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 0);
    ActDeath: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 0);
    );
  MA54                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActCritical: (start: 340; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActDeath: (start: 420; frame: 4; skip: 6; ftime: 200; usetick: 0);
    );

  MA55                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 0; frame: 1; skip: 9; ftime: 0; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA56                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 10; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 0; frame: 1; skip: 9; ftime: 0; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA57                      : TMonsterAction = (
    ActStand: (start: 0; frame: 3; skip: 0; ftime: 160; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 3; frame: 8; skip: 0; ftime: 160; usetick: 0);
    ActCritical: (start: 22; frame: 8; skip: 0; ftime: 160; usetick: 0);
    ActStruck: (start: 0; frame: 1; skip: 9; ftime: 0; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA58                      : TMonsterAction = (
    ActStand: (start: 0; frame: 1; skip: 0; ftime: 160; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActAttack: (start: 1; frame: 34; skip: 0; ftime: 160; usetick: 0);
    ActCritical: (start: 47; frame: 33; skip: 0; ftime: 160; usetick: 0);
    ActStruck: (start: 0; frame: 1; skip: 9; ftime: 0; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    );

  MA59                      : TMonsterAction = (
    ActStand: (start: 0; frame: 10; skip: 0; ftime: 300; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    ActAttack: (start: 0; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    ActStruck: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    );

  MA60                      : TMonsterAction = (
    ActStand: (start: 0; frame: 1; skip: 0; ftime: 300; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    ActAttack: (start: 0; frame: 1; skip: 0; ftime: 150; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    ActStruck: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    ActDie: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    ActDeath: (start: 0; frame: 0; skip: 0; ftime: 150; usetick: 0);
    );

  MA65                      : TMonsterAction = (//»£±‚ø¨
    ActStand: (start: 0; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActWalk: (start: 10; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActAttack: (start: 20; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActCritical: (start: 20; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActStruck: (start: 30; frame: 4; skip: 6; ftime: 100; usetick: 0);
    ActDie: (start: 40; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActDeath: (start: 40; frame: 10; skip: 0; ftime: 150; usetick: 0);
    );
  MA66                      : TMonsterAction = (//∫Òø˘√µ¡÷
    ActStand: (start: 0; frame: 20; skip: 0; ftime: 150; usetick: 0);
    ActWalk: (start: 0; frame: 20; skip: 0; ftime: 150; usetick: 3);
    ActAttack: (start: 20; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActCritical: (start: 20; frame: 10; skip: 0; ftime: 150; usetick: 0);
    ActStruck: (start: 30; frame: 2; skip: 8; ftime: 100; usetick: 0);
    ActDie: (start: 400; frame: 18; skip: 0; ftime: 150; usetick: 0);
    ActDeath: (start: 400; frame: 18; skip: 0; ftime: 150; usetick: 0);
    );
  MA67                      : TMonsterAction = (//»£»•±‚ºÆ
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 0);
    ActWalk: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 3);
    ActAttack: (start: 10; frame: 4; skip: 6; ftime: 300; usetick: 0);
    ActCritical: (start: 10; frame: 4; skip: 6; ftime: 300; usetick: 0);
    ActStruck: (start: 0; frame: 4; skip: 6; ftime: 150; usetick: 0);
    ActDie: (start: 0; frame: 4; skip: 6; ftime: 300; usetick: 0);
    ActDeath: (start: 0; frame: 4; skip: 6; ftime: 300; usetick: 0);
    );

  MA91                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 1040; frame: 15; skip: 0; ftime: 100; usetick: 0);
    );
  MA92                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 1060; frame: 15; skip: 0; ftime: 100; usetick: 0);
    );
  MA93                      : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 100; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 0; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 1080; frame: 15; skip: 0; ftime: 100; usetick: 0);
    );
  MAG25                     : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActCritical: (start: 340; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActDeath: (start: 426; frame: 4; skip: 6; ftime: 120; usetick: 0);
    );
  MAG26                     : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 340; frame: 7; skip: 3; ftime: 120; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActDeath: (start: 422; frame: 4; skip: 6; ftime: 120; usetick: 0);
    );
  MAG27                     : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 340; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 160; usetick: 0);
    ActDeath: (start: 420; frame: 10; skip: 0; ftime: 120; usetick: 0);
    );
  MAG28                     : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 160; usetick: 0);
    ActAttack: (start: 160; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActCritical: (start: 340; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 9; skip: 1; ftime: 160; usetick: 0);
    ActDeath: (start: 420; frame: 4; skip: 6; ftime: 120; usetick: 0);
    );
  MAG29                     : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 110; usetick: 0);
    ActCritical: (start: 340; frame: 8; skip: 2; ftime: 110; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 8; skip: 2; ftime: 120; usetick: 0);
    ActDeath: (start: 420; frame: 7; skip: 3; ftime: 120; usetick: 0);
    );
  MAG30                     : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 110; usetick: 0);
    ActCritical: (start: 340; frame: 8; skip: 2; ftime: 110; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActDeath: (start: 420; frame: 9; skip: 1; ftime: 120; usetick: 0);
    );

  MAG31                     : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 140; usetick: 0);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 110; usetick: 0);
    ActCritical: (start: 340; frame: 8; skip: 2; ftime: 110; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 7; skip: 3; ftime: 120; usetick: 0);
    ActDeath: (start: 420; frame: 7; skip: 3; ftime: 120; usetick: 0);
    );

  MA120                     : TMonsterAction = (
    ActStand: (start: 0; frame: 1; skip: 9; ftime: 200; usetick: 0);
    ActWalk: (start: 0; frame: 0; skip: 0; ftime: 200; usetick: 3);
    ActAttack: (start: 80; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActCritical: (start: 0; frame: 0; skip: 0; ftime: 120; usetick: 0);
    ActStruck: (start: 160; frame: 2; skip: 8; ftime: 100; usetick: 0);
    ActDie: (start: 240; frame: 10; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 240; frame: 10; skip: 0; ftime: 200; usetick: 0);
    );

  MA121                     : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 200; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 340; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 8; skip: 2; ftime: 200; usetick: 0);
    ActDeath: (start: 260; frame: 8; skip: 2; ftime: 200; usetick: 0);
    );

  MA122                     : TMonsterAction = (
    ActStand: (start: 0; frame: 10; skip: 0; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 200; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 340; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 0; ftime: 100; usetick: 0);
    ActDie: (start: 260; frame: 10; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 260; frame: 10; skip: 0; ftime: 200; usetick: 0);
    );

  MA123                     : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 200; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 400; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 8; ftime: 100; usetick: 0);
    ActDie: (start: 320; frame: 10; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 400; frame: 10; skip: 0; ftime: 200; usetick: 0);
    );

  MA123_815                 : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 200; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 400; frame: 10; skip: 0; ftime: 120; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 8; ftime: 100; usetick: 0);
    ActDie: (start: 320; frame: 10; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 400; frame: 10; skip: 0; ftime: 200; usetick: 0);
    );

  MA123_825                 : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 200; usetick: 3);
    ActAttack: (start: 160; frame: 9; skip: 1; ftime: 120; usetick: 0);
    ActCritical: (start: 400; frame: 9; skip: 1; ftime: 120; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 8; ftime: 100; usetick: 0);
    ActDie: (start: 320; frame: 10; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 400; frame: 10; skip: 0; ftime: 200; usetick: 0);
    );

  MA123_827                 : TMonsterAction = (
    ActStand: (start: 0; frame: 4; skip: 6; ftime: 200; usetick: 0);
    ActWalk: (start: 80; frame: 6; skip: 4; ftime: 200; usetick: 3);
    ActAttack: (start: 160; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActCritical: (start: 400; frame: 6; skip: 4; ftime: 120; usetick: 0);
    ActStruck: (start: 240; frame: 2; skip: 8; ftime: 100; usetick: 0);
    ActDie: (start: 320; frame: 10; skip: 0; ftime: 200; usetick: 0);
    ActDeath: (start: 480; frame: 10; skip: 0; ftime: 200; usetick: 0);
    );

  WORDER                    : array[0..1, 0..599] of byte = (
    (
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
    0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1,
    //∞»±‚
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1,
    //∂Ÿ±‚
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1,
    //war∏µÂ
    0, 1, 1, 1, 0, 0, 0, 0,
    //∞¯∞›
    1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1,
    //∞¯∞› 2
    0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1,
    //∞¯∞›3
    1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0,
    1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
    //∏∂π˝
    0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,
    1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1,
    //ùÿ±‚
    0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0,
    //∏¬±‚
    0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1,
    //æ≤∑Ø¡¸
    0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1
    ),

    (
    //¡§¡ˆ
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
    0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1,
    //∞»±‚
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1,
    //∂Ÿ±‚
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1,
    //war∏µÂ
    1, 1, 1, 1, 0, 0, 0, 0,
    //∞¯∞›
    1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1,
    //∞¯∞› 2
    0, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1,
    //∞¯∞›3
    1, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0,
    1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0,
    //∏∂π˝
    0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,
    1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
    0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1,
    //ùÿ±‚
    0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0,
    //∏¬±‚
    0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1,
    //æ≤∑Ø¡¸
    0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1
    )
    );

  EffDir                    : array[0..7] of byte = (0, 0, 1, 1, 1, 1, 1, 0);

type
  TActor = class
    m_btTitleIndex: byte;
    m_RushStep: byte;
    m_btAFilter: Boolean;
    m_btPoisonDecHealth: byte;
    m_nRecogId: Integer;
    n_boState: Boolean;
    m_nIPower: Integer;
    m_nIPowerLvl: Integer;
    m_nIPowerExp: LongWord;
    m_nTagX: Integer;
    m_nTagY: Integer;
    m_nCurrX: Integer;
    m_nCurrY: Integer;
    m_btDir: byte;
    m_btSex: byte;
    m_btRace: byte;
    m_btHair: byte;
    m_btHairEx: byte;
    m_btDress: byte;
    m_btWeapon: byte;
    m_btWeaponEffect: byte;
    m_btHorse: byte;
    m_btEffect: byte;
    m_btAttribute: byte;
    m_btJob: byte;
    m_wGloryPoint: Word;
    m_wAppearance: Word;
    m_btDeathState: byte;
    m_nFeature: Integer;
    m_nFeatureEx: Integer;
    m_wAppr: Word;
    m_nState: Integer;
    m_boDeath: Boolean;
    m_boSkeleton: Boolean;
    m_boItemExplore: Boolean;
    m_boDelActor: Boolean;
    m_boDelActionAfterFinished: Boolean;
    m_sDescUserName: string[255];
    m_sUserName: string;
    m_sUserNameOffSet: Integer;
    //m_sDrawName: array[0..10] of string;
    m_sLoyaly: string[20];
    m_sAutoSayMsg: string[255];
    m_btNameColor: byte;
    m_nNameColor: Integer;
    m_Abil: TAbility;
    m_nGold: Integer;
    m_nGameGold: Integer;
    m_nGameGird: Integer;
    m_nGamePoint: Integer;
    m_nGameDiamd: Integer;
    m_nHitSpeed: shortint;
    m_boVisible: Boolean;
    m_boHoldPlace: Boolean;

    m_SayingArr: array[0..MAXSAY - 1] of string;
    m_SayWidthsArr: array[0..MAXSAY - 1] of Integer;
    m_dwSayTime: LongWord;
    m_nSayX: Integer;
    m_nSayY: Integer;
    m_nSayLineCount: Integer;

    m_nShiftX: shortint;
    m_nShiftY: shortint;

    m_nPx: Integer;
    m_nHpx: Integer;
    m_nWpx: Integer;
    m_nSpx: Integer;

    m_nPy: Integer;
    m_nHpy: Integer;
    m_nWpy: Integer;
    m_nSpy: Integer;

    m_nRx: Integer;
    m_nRy: Integer;

    m_nWpeX: Integer;
    m_nWpeY: Integer;

    m_nDownDrawLevel: Integer;
    m_nTargetX: Integer;
    m_nTargetY: Integer;
    m_nTargetRecog: Integer;
    m_nHiterCode: Integer;
    m_nMagicNum: Integer;
    m_nCurrentEvent: Integer;
    m_boDigFragment: Boolean;
    m_boThrow: Boolean;
    m_boHeroLongHit: Boolean;
    m_boHeroLongHit2: Boolean;
    m_boSmiteHit: Boolean;
    m_boSmiteLongHit: byte;
    m_boSmiteLongHit2: Boolean;
    m_boSmiteLongHitS2: byte;
    m_boSmiteWideHit2: Boolean;
    m_boSmiteWideHitS2: byte;
    m_nBodyOffset: Integer;
    m_nHairOffset: Integer;
    m_nHairOffsetEx: Integer;
    m_nCboHairOffset: Integer;
    m_nHumWinOffset: Integer;
    m_nCboHumWinOffSet: Integer;
    m_nWeaponOffset: Integer;
    m_boUseCboLib: Boolean;
    m_boUseMagic: Boolean;
    m_boNewMagic: Boolean;
    m_boHitEffect: Boolean;
    m_boUseEffect: Boolean;
    m_nHitEffectNumber: Integer;
    m_dwWaitMagicRequest: LongWord;
    m_nWaitForRecogId: Integer;
    m_nWaitForFeature: Integer;
    m_nWaitForStatus: Integer;

    m_nCurEffFrame: Integer;
    m_nSpellFrame: Integer;
    m_CurMagic: TUseMagicInfo;

    m_nGenAniCount: Integer;
    m_boOpenHealth: Boolean;
    m_noInstanceOpenHealth: Boolean;
    m_dwOpenHealthStart: LongWord;
    m_dwOpenHealthTime: LongWord;
    m_BodySurface: TCustomLockableTexture;

    m_boGrouped: Boolean;
    m_nCurrentAction: Integer;
    m_boReverseFrame: Boolean;
    m_boWarMode: Boolean;
    m_dwWarModeTime: LongWord;
    m_nChrLight: Integer;
    m_nMagLight: Integer;
    m_nRushDir: Integer;
    m_nXxI: Integer;
    m_boLockEndFrame: Boolean;
    m_dwLastStruckTime: LongWord;
    m_dwSendQueryUserNameTime: LongWord;
    m_dwDeleteTime: LongWord;

    m_nMagicStruckSound: Integer;
    m_boRunSound: Boolean;
    m_nFootStepSound: Integer;
    m_nStruckSound: Integer;
    m_nStruckWeaponSound: Integer;

    m_nAppearSound: Integer;
    m_nNormalSound: Integer;
    m_nAttackSound: Integer;
    m_nWeaponSound: Integer;
    m_nScreamSound: Integer;
    m_nDieSound: Integer;
    m_nDie2Sound: Integer;

    m_nMagicStartSound: Integer;
    m_nMagicFireSound: Integer;
    m_nMagicExplosionSound: Integer;
    m_Action: pTMonsterAction;

    m_nHeroEnergyType: Integer;
    m_nHeroEnergy: Integer;
    m_nMaxHeroEnergy: Integer;
    m_nBagSize: Integer;
    m_btIsHero: byte;
    m_dwMsgHint: LongWord;
    m_dwHealthHP: LongWord;
    m_dwHealthMP: LongWord;
    m_dwHealthSP: LongWord;
    m_dwHealthBK: LongWord;
    m_dwAutoTecTick: LongWord;
    m_dwAutoTecHeroTick: LongWord;
    m_dwPracticeTick: LongWord;
    m_boFisrShopItem: Boolean;

    m_boAttackSlow: Boolean;            //ÕÛ¡¶≤ªπª ±¬˝∂Ø◊˜π•ª˜.
    m_boMoveSlow: Boolean;              //∏∫÷ÿ≤ªπª ±¬˝∂Ø◊˜≈‹
    m_nMoveSlowLevel: Integer;

    m_StruckDamage: TStringList;
    m_StruckDamage2: TStringList;
    m_StruckDamageTick: LongWord;
    m_StruckDamageTick2: LongWord;
    m_fHideMode: Boolean;
    m_nTempState: byte;
  private
    function GetMessage(ChrMsg: pTChrMsg): Boolean;
  protected
    m_nStartFrame: Integer;
    m_nEndFrame: Integer;

    m_nEffectStart: Integer;
    m_nEffectFrame: Integer;
    m_nEffectEnd: Integer;
    m_dwEffectStartTime: LongWord;
    m_dwEffectFrameTime: LongWord;
    m_dwFrameTime: LongWord;
    m_dwStartTime: LongWord;
    m_nMaxTick: Integer;
    m_nCurTick: Integer;
    m_nMoveStep: Integer;
    m_boMsgMuch: Boolean;
    m_dwStruckFrameTime: LongWord;
    m_nCurrentDefFrame: Integer;
    m_dwDefFrameTime: LongWord;
    m_nDefFrameCount: Integer;
    m_nSkipTick: Integer;
    m_dwSmoothMoveTime: LongWord;
    m_dwGenAnicountTime: LongWord;
    //m_dwLoadSurfaceTime: LongWord;

    m_nOldx: Integer;
    m_nOldy: Integer;
    m_nOldDir: Integer;

    m_nWpord: Integer;
    procedure CalcActorFrame; virtual;
    procedure DefaultMotion; virtual;
    function GetDefaultFrame(wmode: Boolean): Integer; virtual;
    procedure DrawEffSurface(dsurface: TCustomCanvas; source: TCustomLockableTexture; ddx, ddy: Integer; blend: Boolean; ceff: TColorEffect);
    procedure DrawWeaponGlimmer(dsurface: TCustomCanvas; ddx, ddy: Integer);
  public
    m_nCurFocusFrame: Integer;
    m_dwFocusFrameTick: LongWord;
    m_dwLastGetMessageTime: LongWord;

    m_nCurrentFrame: Integer;
    m_dwLoadSurfaceTime: LongWord;
    m_nActBeforeX: Integer;
    m_nActBeforeY: Integer;

    m_ChrMsg: TChrMsg;
    m_MsgList: TGList;                  //list of PTChrMsg
    RealActionMsg: TChrMsg;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SendMsg(wIdent: Word; nX, nY, ndir, nFeature, nState: Integer; sStr: string; nSound: Integer; dwDelay: LongWord = 0);
    procedure UpdateMsg(wIdent: Word; nX, nY, ndir, nFeature, nState: Integer; sStr: string; nSound: Integer);
    procedure CleanUserMsgs;
    procedure ProcMsg;
    procedure ProcHurryMsg;
    function IsIdle: Boolean;
    function ActionFinished: Boolean;
    function CanWalk: Integer;
    function CanRun: Integer;
    function Strucked: Boolean;
    procedure Shift(dir, step, cur, Max: Integer);
    procedure ReadyAction(Msg: TChrMsg);
    function CharWidth: Integer;
    function CharHeight: Integer;
    function CheckSelect(dx, dy: Integer): Boolean;
    procedure CleanCharMapSetting(X, Y: Integer);
    procedure Say(Str: string);
    procedure StruckShowDamage(Str: string);
    procedure StruckShowDamage2(Str: string);
    procedure SetSound; virtual;
    procedure Run; virtual;
    procedure RunSound; virtual;
    procedure RunActSound(frame: Integer); virtual;
    procedure RunFrameAction(frame: Integer); virtual; //«¡∑°¿”∏∂¥Ÿ µ∂∆Ø«œ∞‘ «ÿæﬂ«“¿œ
    procedure ActionEnded(); virtual;
    procedure ReadyNextAction(); virtual;
    function Move({step: Integer}): Boolean;
    procedure MoveFail;
    function CanCancelAction: Boolean;
    procedure CancelAction;
    procedure FeatureChanged; virtual;
    function light: Integer; virtual;
    procedure LoadSurface; virtual;
    function GetDrawEffectValue: TColorEffect;
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); virtual;
    procedure DrawEff(dsurface: TCustomCanvas; dx, dy: Integer); virtual;
    procedure DrawFocus(dsurface: TCustomCanvas);
  end;

  TNpcActor = class(TActor)
  private
    m_nEffX: Integer;                   //0x240
    m_nEffY: Integer;                   //0x244
    m_boDigUp: Boolean;                 //0x248
    m_dwUseEffectTick: LongWord;        //0x24C
    m_EffSurface: TCustomLockableTexture;   //0x250
  public
    constructor Create; override;
    procedure Run; override;
    procedure CalcActorFrame; override;
    function GetDefaultFrame(wmode: Boolean): Integer; override;
    procedure LoadSurface; override;
    procedure DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean = False); override;
    procedure DrawEff(dsurface: TCustomCanvas; dx, dy: Integer); override;
  end;

function GetRaceByPM(race: Integer; Appr: Word): pTMonsterAction;
function GetOffset(Appr: Integer): Integer;
function GetNpcOffset(nAppr: Integer): Integer;

implementation

uses
  ClMain, SoundUtil, clEvent, MShare, HUtil32, HumanActor;

function GetRaceByPM(race: Integer; Appr: Word): pTMonsterAction;
begin
  Result := nil;
  case race of
    9 {01}: Result := @MA9;             //475D70
    10 {02}: Result := @MA10;           //475D7C
    11 {03}: Result := @MA11;           //475D88
    12 {04}: Result := @MA12;           //475D94
    13 {05}: Result := @MA13;           //475DA0
    14 {06}: Result := @MA14;           //475DAC
    15 {07}: Result := @MA15;           //475DB8
    16 {08}: Result := @MA16;           //475DC4
    17 {06}: Result := @MA14;           //475DAC
    18 {06}: Result := @MA14;           //475DAC
    19 {0A}: Result := @MA19;           //475DDC
    20 {0A}: Result := @MA19;           //475DDC
    21 {0A}: Result := @MA19;           //475DDC
    22 {07}: Result := @MA15;           //475DB8
    23 {06}: Result := @MA14;           //475DAC
    24 {04}: Result := @MA12;           //475D94
    25 {04}: Result := @MAG25;
    26 {04}: Result := @MAG26;
    27 {04}: Result := @MAG27;
    28 {04}: Result := @MAG28;
    29 {04}: Result := @MAG29;

    30 {09}: Result := @MA17;           //475DD0
    31 {09}: Result := @MA17;           //475DD0
    32 {0F}: Result := @MA24;           //475E18
    33 {10}: Result := @MA25;           //475E24
    34 {11}: Result := @MA30;           //475E30  ≥‡‘¬∂Òƒß
    35 {12}: Result := @MA31;           //475E3C
    36 {13}: Result := @MA32;           //475E48
    37 {0A}: Result := @MA19;           //475DDC
    38 {04}: Result := @MAG29;
    39 {04}: Result := @MAG30;

    40 {0A}: Result := @MA19;           //475DDC
    41 {0B}: Result := @MA20;           //475DE8
    42 {0B}: Result := @MA20;           //475DE8
    43 {0C}: Result := @MA21;           //475DF4
    44 {0C}: Result := @MAG31;
    45 {0A}: Result := @MA19;           //475DDC
    46 {0A}: Result := @MA50;
    47 {0D}: Result := @MA22;           //475E00
    48 {0E}: Result := @MA23;           //475E0C
    49 {0E}: Result := @MA23;           //475E0C
    50 {27}: begin                      //475F32
        case Appr of
          23 {01}: Result := @MA36;     //475F77
          24 {02}: Result := @MA37;     //475F80 no Act
          25 {02}: Result := @MA37;     //475F80

          27 {02}: Result := @MA37;     //475F80

          32 {02}: Result := @MA37;     //475F80
          33 {02}: Result := @MA35;     //475F80

          35 {03}: Result := @MA41;     //475F89
          36 {03}: Result := @MA41;     //475F89
          37 {03}: Result := @MA41;     //475F89
          38 {03}: Result := @MA41;     //475F89
          39 {03}: Result := @MA41;     //475F89
          40 {03}: Result := @MA41;     //475F89
          41 {03}: Result := @MA41;     //475F89
          42 {04}: Result := @MA46;     //475F92
          43 {04}: Result := @MA46;     //475F92
          44 {04}: Result := @MA46;     //475F92
          45 {04}: Result := @MA46;     //475F92
          46 {04}: Result := @MA46;     //475F92
          47 {04}: Result := @MA46;     //475F92
          48 {03}: Result := @MA41;     //4777B3
          49 {03}: Result := @MA41;     //4777B3
          50 {03}: Result := @MA41;     //4777B3

          52 {03}: Result := @MA41;     //4777B3
          53 {03}: Result := @MA41;
          54..058: Result := @MA59;
          94..098: Result := @MA59;
          59: Result := @MA60;
          60..068: Result := @MA55;
          70..075: Result := @MA55;
          76..080: Result := @MA35;     //norm
          81..083: Result := @MA56;     //hero
          84: Result := @MA57;
          85: Result := @MA58;
          90..092: Result := @MA55;

          111: Result := @MA111;
          132: Result := @MA131;

        else
          Result := @MA35;
        end;
      end;
    51 {0A}: Result := @MA50;
    52 {0A}: Result := @MA19;           //475DDC
    53 {0A}: Result := @MA19;           //475DDC
    54 {14}: Result := @MA28;           //475E54
    55 {15}: Result := @MA29;           //475E60
    56 {22}: Result := @MA43;           //475EFC
    57 {22}: Result := @MA15;
    58 {22}: Result := @MA15;

    60 {16}: Result := @MA33;           //475E6C
    61 {16}: Result := @MA33;           //475E6C
    62 {16}: Result := @MA33;           //475E6C
    63 {17}: Result := @MA34;           //475E78
    64 {18}: Result := @MA19;           //475E84
    65 {18}: Result := @MA19;           //475E84
    66 {18}: Result := @MA19;           //475E84
    67 {18}: Result := @MA19;           //475E84
    68 {18}: Result := @MA19;           //475E84
    69 {18}: Result := @MA19;           //475E84
    70 {19}: Result := @MA33;           //475E90
    71 {19}: Result := @MA33;           //475E90
    72 {19}: Result := @MA33;           //475E90
    73 {1A}: Result := @MA19;           //475E9C
    74 {1B}: Result := @MA19;           //475EA8
    75 {1C}: Result := @MA39;           //475EB4
    76 {1D}: Result := @MA38;           //475EC0
    77 {1E}: Result := @MA39;           //475ECC
    78 {1F}: Result := @MA40;           //475ED8
    79 {20}: Result := @MA19;           //475EE4
    80 {21}: Result := @MA42;           //475EF0
    81 {22}: Result := @MA43;           //475EFC
    83 {23}: Result := @MA44;           //475F08
    84 {24}: Result := @MA47;           //475F14
    85 {24}: Result := @MA47;           //475F14
    86 {24}: Result := @MA47;           //475F14
    87 {24}: Result := @MA47;           //475F14
    88 {24}: Result := @MA47;           //475F14
    89 {24}: Result := @MA47;           //475F14
    90 {11}: Result := @MA47;           //475E30
    91 {06}: Result := @MA91;
    92 {06}: Result := @MA92;
    93 {06}: Result := @MA93;
    94 {14}: Result := @MA28;           //475E54
    95 {15}: Result := @MA29;           //475E60

    98 {25}: Result := @MA27;           //475F20
    99 {26}: Result := @MA26;           //475F29
    101 {19}: Result := @MA33;          //475E90
    102: Result := @MA48;
    103: Result := @MA49;
    104: Result := @MA49;
    105: Result := @MA49;
    106: Result := @MA50;
    109: Result := @MA51;

    110: Result := @MA50;
    111: Result := @MA54;

    113..115: Result := @MA33;
    117: Result := @MA67;
    118, 119: Result := @MA65;
    120: Result := @MA66;

    121: Result := @MA121;
    122: Result := @MA122;
    123: begin
        case Appr of
          812: Result := @MA120;
          815: Result := @MA123_815;
          825: Result := @MA123_825;
          827: Result := @MA123_827;
        else
          Result := @MA123;
        end;
      end
  else
    Result := @MA19;
  end
end;

function GetOffset(Appr: Integer): Integer;
var
  nRace, nPos               : Integer;
begin
  Result := 0;
  if (Appr >= 1000) then Exit;
  nRace := Appr div 10;
  nPos := Appr mod 10;
  case nRace of
    0: Result := nPos * 280;
    1: Result := nPos * 230;
    2, 3, 7..12: Result := nPos * 360;
    4: begin
        Result := nPos * 360;
        if nPos = 1 then Result := 600;
      end;
    5: Result := nPos * 430;
    6: Result := nPos * 440;
    13: case nPos of
        0: Result := 0;
        1: Result := 360;
        2: Result := 440;
        3: Result := 550;
      else
        Result := nPos * 360;
      end;
    14: Result := nPos * 360;
    15: Result := nPos * 360;
    16: Result := nPos * 360;
    17: case nPos of
        2: Result := 920;
      else Result := nPos * 350;
      end;
    18: case nPos of
        0: Result := 0;
        1: Result := 520;
        2: Result := 950;
      else
        Result := 494 + nPos * 360;
      end;
    19: case nPos of
        0: Result := 0;
        1: Result := 370;
        2: Result := 810;
        3: Result := 1250;
        4: Result := 1630;
        5: Result := 2010;
        6: Result := 2390;
      end;
    20: case nPos of
        0: Result := 0;
        1: Result := 360;
        2: Result := 720;
        3: Result := 1080;
        4: Result := 1440;
        5: Result := 1800;
        6: Result := 2350;
        7: Result := 3060;
      end;
    21: case nPos of
        0: Result := 0;
        1: Result := 460;
        2: Result := 820;
        3: Result := 1180;
        4: Result := 1540;
        5: Result := 1900;
        6: Result := 2440;
        7: Result := 2570;
      else
        Result := 2700;
      end;
    22: case nPos of
        0: Result := 0;
        1: Result := 430;
        2: Result := 1290;
        3: Result := 1810;
        4: Result := 2320;
        5: Result := 2920;
        6: Result := 3270;
        7: Result := 3620;
      end;
    23: case nPos of
        0: Result := 0;
        1: Result := 340;
        2: Result := 680;
        3: Result := 1180;              //Fox mob 3
        4: Result := 1770;              //Fox mob 4
        5: Result := 2610;              //Fox mob 5
        6: Result := 2950;              //Fox mob 6
        7: Result := 3290;              //Fox mob 7
        8: Result := 3750;
        9: Result := 4460;
      end;
    24: case nPos of
        0: Result := 0;
        1: Result := 510;
        2: Result := 1090;
      else
        Result := 510 * (nPos + 1);
      end;
    25: case nPos of
        0: Result := 0;
        1: Result := 510;
        2: Result := 1020;
        3: Result := 1370;
        4: Result := 1720;
        5: Result := 2070;
        6: Result := 2740;
        7: Result := 3780;
        8: Result := 3820;
        9: Result := 4170;
      end;
    26: case nPos of
        0: Result := 0;
        1: Result := 340;
        2: Result := 680;
        3: Result := 1190;
        4: Result := 1930;
        5: Result := 2100;
        6: Result := 2440;

        7: Result := 2540;
        8: Result := 3570;
      end;

    28: case nPos of
        0: Result := 0;
      end;
    29: Result := 360 * nPos;

    32: case nPos of                    //mon33
        0: Result := 0;
        1: Result := 440;
        2: Result := 820;
        3: Result := 1360;
        4: Result := 2650;
        5: Result := 2680;
        6: Result := 2790;
        7: Result := 2900;
        8: Result := 3500;
        9: Result := 3930;
      end;
    33: case nPos of                    //mon34
        0: Result := 20;
        1: Result := 720;
        2: Result := 1160;
        3: Result := 1770;
        4: Result := 1780;
        5: Result := 1790;
        6: Result := 1840;
        7: Result := 2540;
        8: Result := 2900;
      else
        Result := (nPos - 7) * 360 + 2900;
      end;
    34: case nPos of                    //mon35
        0: Result := 0;
        1: Result := 680;
        2: Result := 1030;
      else
        Result := (nPos - 6) * 360 + 1800;
      end;

    35: case nPos of                    //mon35
        0: Result := 0;
        1: Result := 810;
        2: Result := 1800;
        3: Result := 2610;
        4: Result := 3420;
        5: Result := 4390;
        6: Result := 5200;
        7: Result := 6170;
        8: Result := 6980;
        9: Result := 7790;
      end;
    81: case nPos of
        0: Result := 8760;
        1: Result := 9570;
        2: Result := 10380;             //...
        3: Result := 11030;
        4: Result := 12000;
        5: Result := 13800;
        6: Result := 14770;
        7: Result := 15580;
        8: Result := 16390;
        9: Result := 17360;
      end;
    82: case nPos of
        0: Result := 18330;
        1: Result := 19300;
        2: Result := 20270;
        3: Result := 21240;
        4: Result := 22050;
        5: Result := 22860;
        6: Result := 23990;
        7: Result := 24800;
        8: Result := 25930;
      end;

    70: case nPos of
        0: Result := 0;
        1: Result := 360;
        2: Result := 720;
        3: Result := 0;
        4: Result := 350;
        5: Result := 780;
        6: Result := 1130;
        7: Result := 1560;
        8: Result := 1910;
      end;
    80: case nPos of
        0: Result := 0;
        1: Result := 80;
        2: Result := 300;
        3: Result := 301;
        4: Result := 302;
        5: Result := 320;
        6: Result := 321;
        7: Result := 322;
        8: Result := 321;
      end;
    90: case nPos of
        0: Result := 80;
        1: Result := 168;
        2: Result := 184;
        3: Result := 200;
        4: Result := 1770;
        5: Result := 1780;
        6: Result := 1790;
      end;
  else
    Result := nPos * 360;
  end;

end;

function GetNpcOffset(nAppr: Integer): Integer;
begin
  Result := 0;
  case nAppr of
    0..22: Result := nAppr * 60;
    23: Result := 1380;
    24, 25: Result := (nAppr - 24) * 60 + 1470;
    27, 32: Result := (nAppr - 26) * 60 + 1620 - 30;
    26, 28, 29, 30, 31, 33..41: Result := (nAppr - 26) * 60 + 1620;
    42, 43: Result := 2580;
    44..47: Result := 2640;
    48..50: Result := (nAppr - 48) * 60 + 2700;
    51: Result := 2880;
    52: Result := 2960;
    54..058: Result := 4490 + 10 * (nAppr - 54);
    94..098: Result := 4490 + 10 * (nAppr - 94);
    59: Result := 4540;
    60..67: Result := 3060 + 60 * (nAppr - 60);
    68: Result := 3600;
    70..75: Result := 3780 + 10 * (nAppr - 70);
    76..77: Result := 3840 + 60 * (nAppr - 76);
    81..83: Result := 3960 + 20 * (nAppr - 81);
    78..80: Result := 4060 + 60 * (nAppr - 78);
    84: Result := 4030;
    85: Result := 4250;
    90..92: Result := 3750 + 10 * (nAppr - 90);
    100..123: begin
        case nAppr of
          111: Result := 740;

          112: Result := 810;
          113: Result := 820;
          114: Result := 830;
          115: Result := 840;
          116: Result := 850;
          117: Result := 860;

          118: Result := 870;           //
          119: Result := 900;           //
          120: Result := 930;           //

          121: Result := 970;
          122: Result := 980;           //
          123: Result := 990;           //
        else
          Result := (nAppr - 100) * 70;
        end;
      end;

    130: Result := 4240;
    131: Result := 4560;
    132: Result := 4770;
    133: Result := 4810;
  end;
end;

constructor TActor.Create;
var
  i                         : Integer;
begin
  inherited Create;
  m_btTitleIndex := 0;
  m_nCurFocusFrame := 0;
  FillChar(m_Abil, SizeOf(TAbility), 0);
  FillChar(m_Action, SizeOf(m_Action), 0);
  m_nWaitForRecogId := 0;
  m_MsgList := TGList.Create;
  m_nRecogId := 0;
  m_wAppr := 0;
  m_btPoisonDecHealth := 0;
  n_boState := False;
  m_btAFilter := False;
  m_nIPower := -1;
  m_nIPowerLvl := 0;
  m_nIPowerExp := 0;
  m_btAttribute := 0;
  m_BodySurface := nil;
  m_nGold := 0;
  m_boVisible := True;
  m_boHoldPlace := True;
  m_wGloryPoint := 0;
  m_nCurrentAction := 0;
  m_boReverseFrame := False;
  m_nShiftX := 0;
  m_nShiftY := 0;
  m_nDownDrawLevel := 0;
  m_nCurrentFrame := -1;
  m_nEffectFrame := -1;
  RealActionMsg.ident := 0;
  m_sUserName := '';
  m_sUserNameOffSet := 0;
  //for i := Low(m_sDrawName) to High(m_sDrawName) do m_sDrawName[i] := '';
  m_sAutoSayMsg := '';
  m_btNameColor := 255;
  m_nNameColor := clWhite;
  m_dwSendQueryUserNameTime := GetTickCount;
  m_boWarMode := False;
  m_dwWarModeTime := 0;
  m_boDeath := False;
  m_boSkeleton := False;
  m_boItemExplore := False;
  m_boDelActor := False;
  m_boDelActionAfterFinished := False;

  m_nChrLight := 0;
  m_nMagLight := 0;
  m_boLockEndFrame := False;
  m_dwSmoothMoveTime := 0;
  m_dwGenAnicountTime := 0;
  m_dwDefFrameTime := 0;
  m_dwLoadSurfaceTime := GetTickCount;
  m_boGrouped := False;
  m_boOpenHealth := False;
  m_noInstanceOpenHealth := False;
  m_CurMagic.ServerMagicCode := 0;
  m_nSpellFrame := DEFSPELLFRAME;
  m_nNormalSound := -1;
  m_nFootStepSound := -1;
  m_nAttackSound := -1;
  m_nWeaponSound := -1;
  m_nStruckSound := s_struck_body_longstick;
  m_nStruckWeaponSound := -1;
  m_nScreamSound := -1;
  m_nDieSound := -1;
  m_nDie2Sound := -1;
  m_btIsHero := 0;
  m_boFisrShopItem := True;

  m_boSmiteLongHit := 0;

  m_boSmiteLongHit2 := False;
  m_boSmiteLongHitS2 := 0;
  m_boSmiteWideHit2 := False;
  m_boSmiteWideHitS2 := 0;

  m_boAttackSlow := False;
  m_boMoveSlow := False;
  m_nMoveSlowLevel := 0;
  m_btWeaponEffect := 0;

  m_StruckDamage := TStringList.Create;
  m_StruckDamage2 := TStringList.Create;
  m_nTempState := 1;
  m_fHideMode := False;
  m_dwLastGetMessageTime := GetTickCount;
end;

destructor TActor.Destroy;
var
  i                         : Integer;
  Msg                       : pTChrMsg;
begin
  for i := 0 to m_MsgList.count - 1 do begin
    Msg := m_MsgList.Items[i];
    Dispose(Msg);
  end;
  m_MsgList.Free;
  m_StruckDamage.Free;
  m_StruckDamage2.Free;
  inherited Destroy;
end;

procedure TActor.SendMsg(wIdent: Word; nX, nY, ndir, nFeature, nState: Integer; sStr: string; nSound: Integer; dwDelay: LongWord);
var
  Msg                       : pTChrMsg;
begin
  New(Msg);
  Msg.ident := wIdent;
  Msg.X := nX;
  Msg.Y := nY;
  Msg.dir := ndir;
  Msg.Feature := nFeature;
  Msg.State := nState;
  Msg.saying := Str_ToInt(sStr, 0);
  Msg.sound := nSound;
  if dwDelay > 0 then
    Msg.dwDelay := GetTickCount + dwDelay
  else
    Msg.dwDelay := 0;
  m_MsgList.Lock;
  try
    //111
    m_MsgList.Add(Msg);
  finally
    m_MsgList.UnLock;
  end;
end;

procedure TActor.UpdateMsg(wIdent: Word; nX, nY, ndir, nFeature, nState: Integer; sStr: string; nSound: Integer);
var
  i                         : Integer;
  Msg                       : pTChrMsg;
begin
  m_MsgList.Lock;
  try
    i := 0;
    while True do begin
      if i >= m_MsgList.count then Break;
      Msg := m_MsgList.Items[i];
      if ((Self = g_MySelf) and (Msg.ident >= 3000) and (Msg.ident <= 3099)) or (Msg.ident = wIdent) then begin
        Dispose(Msg);
        m_MsgList.Delete(i);
        Continue;
      end;
      Inc(i);
    end;
  finally
    m_MsgList.UnLock;
  end;
  SendMsg(wIdent, nX, nY, ndir, nFeature, nState, sStr, nSound);
end;

procedure TActor.CleanUserMsgs;
var
  i                         : Integer;
  Msg                       : pTChrMsg;
begin
  m_MsgList.Lock;
  try
    i := 0;
    while True do begin
      if i >= m_MsgList.count then Break;
      Msg := m_MsgList.Items[i];
      if (Msg.ident >= 3000) and (Msg.ident <= 3099) then begin
        Dispose(Msg);
        m_MsgList.Delete(i);
        Continue;
      end;
      Inc(i);
    end;
  finally
    m_MsgList.UnLock;
  end;
end;

procedure TActor.CalcActorFrame;
var
  haircount                 : Integer;
begin
  m_boUseCboLib := False;
  m_boUseMagic := False;
  m_boNewMagic := False;
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetOffset(m_wAppearance);
  if g_gcGeneral[12] and (m_btRace <> 50) then m_Action := GetRaceByPM(18, 27)
  else m_Action := GetRaceByPM(m_btRace, m_wAppearance);
  if m_Action = nil then Exit;
  case m_nCurrentAction of
    SM_TURN: begin
        m_nStartFrame := m_Action.ActStand.start + m_btDir * (m_Action.ActStand.frame + m_Action.ActStand.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActStand.frame - 1;
        m_dwFrameTime := m_Action.ActStand.ftime;
        m_dwStartTime := GetTickCount;
        m_nDefFrameCount := m_Action.ActStand.frame;
        Shift(m_btDir, 0, 0, 1);
        if m_fHideMode then begin
          m_fHideMode := False;
          m_dwSmoothMoveTime := 0;
          m_nCurrentAction := 0;
        end;
      end;
    SM_WALK, SM_RUSH, SM_RUSHKUNG, SM_BACKSTEP: begin
        m_nStartFrame := m_Action.ActWalk.start + m_btDir * (m_Action.ActWalk.frame + m_Action.ActWalk.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActWalk.frame - 1;
        m_dwFrameTime := m_Action.ActWalk.ftime;
        m_dwStartTime := GetTickCount;
        m_nMaxTick := m_Action.ActWalk.usetick;
        m_nCurTick := 0;
        m_nMoveStep := 1;
        if m_nCurrentAction = SM_BACKSTEP then
          Shift(GetBack(m_btDir), m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1)
        else
          Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);
      end;
    SM_HIT: begin
        m_nStartFrame := m_Action.ActAttack.start + m_btDir * (m_Action.ActAttack.frame + m_Action.ActAttack.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActAttack.frame - 1;
        m_dwFrameTime := m_Action.ActAttack.ftime;
        m_dwStartTime := GetTickCount;
        m_dwWarModeTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_STRUCK: if (m_btRace <> 12) then begin
        m_nStartFrame := m_Action.ActStruck.start + m_btDir * (m_Action.ActStruck.frame + m_Action.ActStruck.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActStruck.frame - 1;
        m_dwFrameTime := m_dwStruckFrameTime;
        m_dwStartTime := GetTickCount;
        Shift(m_btDir, 0, 0, 1);
      end;
    SM_DEATH: begin
        m_nStartFrame := m_Action.ActDie.start + m_btDir * (m_Action.ActDie.frame + m_Action.ActDie.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActDie.frame - 1;
        m_nStartFrame := m_nEndFrame;
        m_dwFrameTime := m_Action.ActDie.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_NOWDEATH: begin
        m_nStartFrame := m_Action.ActDie.start + m_btDir * (m_Action.ActDie.frame + m_Action.ActDie.skip);
        m_nEndFrame := m_nStartFrame + m_Action.ActDie.frame - 1;
        m_dwFrameTime := m_Action.ActDie.ftime;
        m_dwStartTime := GetTickCount;
      end;
    SM_SKELETON: begin
        m_nStartFrame := m_Action.ActDeath.start + m_btDir;
        m_nEndFrame := m_nStartFrame + m_Action.ActDeath.frame - 1;
        m_dwFrameTime := m_Action.ActDeath.ftime;
        m_dwStartTime := GetTickCount;
      end;
  end;
end;

procedure TActor.ReadyAction(Msg: TChrMsg);
var
  n, T                      : Integer;
  UseMagic                  : PTUseMagicInfo;
begin
  m_nActBeforeX := m_nCurrX;
  m_nActBeforeY := m_nCurrY;
  if Msg.ident = SM_ALIVE then begin
    g_boViewFog := g_boLastViewFog;
    m_boDeath := False;
    m_boSkeleton := False;
    m_boItemExplore := False;
  end;
  if not m_boDeath then begin
    case Msg.ident of
      SM_TURN, SM_WALK, SM_BACKSTEP, SM_RUSH, SM_RUSHEX, SM_RUSHKUNG, SM_RUN, SM_HORSERUN, SM_DIGUP, SM_ALIVE: begin

          //m_nFeature := MakeHumanFeature(0, DRESSfeature(Msg.Feature), WEAPONfeature(Msg.Feature), 6 * 10 + 1 * 2 + 1); //Msg.Feature;
          m_nFeature := Msg.Feature;
          m_nState := Msg.State;
          m_RushStep := HiByte(Msg.dir);
          if m_nState and STATE_OPENHEATH <> 0 then
            m_boOpenHealth := True
          else
            m_boOpenHealth := False;
        end;
    end;
    if Msg.ident = SM_LIGHTING then
      n := 0;
    if g_MySelf = Self then begin
      if (Msg.ident = CM_WALK) then
        if not g_PlayScene.CanWalk(Msg.X, Msg.Y) then
          Exit;
      if (Msg.ident = CM_RUN) then
        if not g_PlayScene.CanRun(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, Msg.X, Msg.Y) then
          Exit;
      if (Msg.ident = CM_HORSERUN) then
        if not g_PlayScene.CanRun(g_MySelf.m_nCurrX, g_MySelf.m_nCurrY, Msg.X, Msg.Y) then
          Exit;
      case Msg.ident of
        CM_TURN,
          CM_WALK,
          CM_SITDOWN,
          CM_RUN,
          CM_HIT,
          CM_HEAVYHIT,
          CM_BIGHIT,
          CM_POWERHIT,
          CM_LONGHIT,
          CM_SQUHIT,
          CM_WIDEHIT,
          CM_CRSHIT,
          CM_TWNHIT,
          CM_PURSUEHIT,
          CM_SMITEHIT,
          CM_SMITELONGHIT,
          CM_SMITELONGHIT2,
          CM_SMITELONGHIT3,
          CM_SMITEWIDEHIT,
          CM_SMITEWIDEHIT2,
          CM_HERO_LONGHIT2: begin
            if Msg.ident = CM_POWERHIT then begin
              Msg.saying := GetMagicLv(Self, 7); //Format('%d', [GetMagicLv(Self, 7)]);
            end else if Msg.ident = CM_LONGHIT then
              Msg.saying := GetMagicLv(Self, 12) //Format('%d', [GetMagicLv(Self, 12)])
            else if Msg.ident = CM_WIDEHIT then
              Msg.saying := GetMagicLv(Self, 25)
            else if Msg.ident = CM_PURSUEHIT then
              Msg.saying := GetMagicLv(Self, 56);
            RealActionMsg := Msg;
            Msg.ident := Msg.ident - 3000;
          end;
        CM_HORSERUN: begin
            RealActionMsg := Msg;
            Msg.ident := SM_HORSERUN;
          end;
        CM_THROW: begin
            if m_nFeature <> 0 then begin
              m_nTargetX := TActor(Msg.Feature).m_nCurrX;
              m_nTargetY := TActor(Msg.Feature).m_nCurrY;
              m_nTargetRecog := TActor(Msg.Feature).m_nRecogId;
            end;
            RealActionMsg := Msg;
            Msg.ident := SM_THROW;
          end;
        CM_FIREHIT: begin
            Msg.saying := GetMagicLv(Self, 26); //Format('%d', [GetMagicLv(Self, 26)]);
            RealActionMsg := Msg;
            Msg.ident := SM_FIREHIT;
          end;
        CM_SPELL: begin
            if g_MagicTarget <> nil then Msg.dir := GetFlyDirection(m_nCurrX, m_nCurrY, g_MagicTarget.m_nCurrX, g_MagicTarget.m_nCurrY);
            RealActionMsg := Msg;
            UseMagic := PTUseMagicInfo(Msg.Feature);
            RealActionMsg.dir := UseMagic.MagicSerial;
            Msg.ident := Msg.ident - 3000;
            Msg.X := GetMagicLv(Self, UseMagic.MagicSerial);
            Msg.Y := m_btPoisonDecHealth;
          end;
      end;

      m_nOldx := m_nCurrX;
      m_nOldy := m_nCurrY;
      m_nOldDir := m_btDir;
    end;

    case Msg.ident of
      SM_STRUCK: begin
          m_nMagicStruckSound := Msg.X;
          n := Round(200 - m_Abil.Level * 5);
          if n > 80 then m_dwStruckFrameTime := n
          else m_dwStruckFrameTime := 80;
          m_dwLastStruckTime := GetTickCount;
        end;
      SM_SPELL: begin
          m_btDir := Msg.dir;
          UseMagic := PTUseMagicInfo(Msg.Feature);
          if UseMagic <> nil then begin
            m_CurMagic := UseMagic^;
            m_CurMagic.ServerMagicCode := -1;
            m_CurMagic.targx := Msg.X;
            m_CurMagic.targy := Msg.Y;
            m_CurMagic.spelllv := Msg.X;
            m_CurMagic.Poison := Msg.Y;
            Dispose(UseMagic);
            /////////////////
            if m_CurMagic.EffectNumber in [60..66] then begin
              //m_CurMagic.ServerMagicCode := 0;
              g_SeriesSkillFire := False;
              g_SeriesSkillFire_100 := False;
            end;
          end;
        end;
      SM_FIREHIT, SM_POWERHIT, SM_LONGHIT, SM_WIDEHIT, SM_PURSUEHIT: begin
          m_nCurrX := Msg.X;
          m_nCurrY := Msg.Y;
          m_btDir := Msg.dir;
          m_CurMagic.magfirelv := Msg.saying; //Str_ToInt(Msg.saying, 0);
        end;
    else begin
        m_nCurrX := Msg.X;
        m_nCurrY := Msg.Y;
        m_btDir := Msg.dir;
      end;
    end;
    m_nCurrentAction := Msg.ident;
    CalcActorFrame;
  end else if Msg.ident = SM_SKELETON then begin
    m_nCurrentAction := Msg.ident;
    CalcActorFrame;
    m_boSkeleton := True;
  end;
  if (Msg.ident = SM_DEATH) or (Msg.ident = SM_NOWDEATH) then begin
    m_boDeath := True;
    if HiByte(Msg.dir) <> 0 then
      m_boItemExplore := True;
    g_PlayScene.ActorDied(Self);
  end;
  RunSound;
end;

function TActor.GetMessage(ChrMsg: pTChrMsg): Boolean;
var
  i                         : Integer;
  Msg                       : pTChrMsg;
begin
  Result := False;
  m_MsgList.Lock;
  try
    i := 0;
    while m_MsgList.count > i do begin
      Msg := m_MsgList.Items[i];
      if (Msg.dwDelay <> 0) and (GetTickCount < Msg.dwDelay) then begin
        Inc(i);
        Continue;
      end;
      ChrMsg^ := Msg^;
      Dispose(Msg);
      m_MsgList.Delete(i);
      Result := True;
      Break;
    end;
  finally
    m_MsgList.UnLock;
  end;
end;

procedure TActor.ProcMsg;
var
  //Msg                       : TChrMsg;
  meff                      : TMagicEff;
begin
  while (m_nCurrentAction = 0) and GetMessage(@m_ChrMsg) do begin
    m_dwLastGetMessageTime := GetTickCount;
    case m_ChrMsg.ident of
      SM_STRUCK: begin
          m_nHiterCode := m_ChrMsg.sound;
          ReadyAction(m_ChrMsg);
        end;
      SM_DEATH, SM_NOWDEATH, SM_SKELETON, SM_ALIVE,
        SM_ACTION_MIN..SM_ACTION_MAX,
        SM_ACTION2_MIN..SM_ACTION2_MAX,
        3000..3099: ReadyAction(m_ChrMsg);
      SM_SPACEMOVE_HIDE: begin
          meff := TScrollHideEffect.Create(250, 10, m_nCurrX, m_nCurrY, Self);
          g_PlayScene.m_EffectList.Add(meff);
          g_SndMgr.PlaySound(s_spacemove_out, m_nCurrX, m_nCurrY);
        end;
      SM_SPACEMOVE_HIDE2: begin
          meff := TScrollHideEffect.Create(1590, 10, m_nCurrX, m_nCurrY, Self);
          g_PlayScene.m_EffectList.Add(meff);
          g_SndMgr.PlaySound(s_spacemove_out, m_nCurrX, m_nCurrY);
        end;
      SM_SPACEMOVE_SHOW: begin
          meff := TCharEffect.Create(260, 10, Self);
          g_PlayScene.m_EffectList.Add(meff);
          m_ChrMsg.ident := SM_TURN;
          ReadyAction(m_ChrMsg);
          g_SndMgr.PlaySound(s_spacemove_in, m_nCurrX, m_nCurrY);
        end;
      SM_SPACEMOVE_SHOW2: begin
          meff := TCharEffect.Create(1600, 10, Self);
          g_PlayScene.m_EffectList.Add(meff);
          m_ChrMsg.ident := SM_TURN;
          ReadyAction(m_ChrMsg);
          g_SndMgr.PlaySound(s_spacemove_in, m_nCurrX, m_nCurrY);
        end;
    end;
  end;
end;

procedure TActor.ProcHurryMsg;
var
  n                         : Integer;
  Msg                       : TChrMsg;
  fin                       : Boolean;
begin
  m_MsgList.Lock;
  try
    n := 0;
    while True do begin
      if m_MsgList.count <= n then Break;
      Msg := pTChrMsg(m_MsgList[n])^;
      fin := False;
      case Msg.ident of
        SM_MAGICFIRE:
          if m_CurMagic.ServerMagicCode <> 0 then begin
            m_CurMagic.ServerMagicCode := 255;
            m_CurMagic.target := Msg.X;
            if Msg.Y in [0..MAXMAGICTYPE - 1] then
              m_CurMagic.EffectType := TMagicType(Msg.Y); //EffectType
            m_CurMagic.EffectNumber := Msg.dir mod 255; //Effect
            m_CurMagic.targx := Msg.Feature;
            m_CurMagic.targy := Msg.State;
            m_CurMagic.magfirelv := Msg.saying; //Str_ToInt(Msg.saying, 0);
            m_CurMagic.Recusion := True;
            fin := True;
          end;
        SM_MAGICFIRE_FAIL:
          if m_CurMagic.ServerMagicCode <> 0 then begin
            m_CurMagic.ServerMagicCode := 0;
            fin := True;
          end;
      end;
      if fin then begin
        Dispose(pTChrMsg(m_MsgList[n]));
        m_MsgList.Delete(n);
      end else
        Inc(n);
    end;
  finally
    m_MsgList.UnLock;
  end;
end;

function TActor.IsIdle: Boolean;
begin
  Result := (m_nCurrentAction = 0) and (m_MsgList.count = 0);
  { then
    Result := True
  else
    Result := False;}
end;

function TActor.ActionFinished: Boolean;
begin
  if (m_nCurrentAction = 0) or (m_nCurrentFrame >= m_nEndFrame) then
    Result := True
  else
    Result := False;
end;

function TActor.CanWalk: Integer;
begin
  if (GetTickCount - g_dwLatestSpellTick < g_dwMagicPKDelayTime) then
    Result := -1
  else
    Result := 1;
end;

function TActor.CanRun: Integer;
begin
  Result := 1;
  if m_Abil.HP < RUN_MINHEALTH then begin
    Result := -1;
  end {else if (GetTickCount - LastStruckTime < 3 * 1000) or (GetTickCount - LatestSpellTime < MagicPKDelayTime) then
    Result := -2;}
end;

function TActor.Strucked: Boolean;
var
  i                         : Integer;
begin
  Result := False;
  m_MsgList.Lock;
  try
    for i := 0 to m_MsgList.count - 1 do begin
      if pTChrMsg(m_MsgList[i]).ident = SM_STRUCK then begin
        Result := True;
        Break;
      end;
    end;
  finally
    m_MsgList.UnLock;
  end;
end;

//Shift(m_btDir, m_nMoveStep, 0, m_nEndFrame - m_nStartFrame + 1);

//“ı”∞…¡À∏–ﬁ∏¥  ÷±Ω”ÃÊªª∫Ø ˝
procedure TActor.Shift(dir, step, cur, max: integer);
var
  unx, uny, ss, v: Integer;
  funx, funy:Integer;
begin
  unx := UNITX * step;
  uny := UNITY * step;
  if cur > max then cur := max;
  m_nRx := m_nCurrX;
  m_nRy := m_nCurrY;
//   ss := Round((max-cur-1) / max) * step;
  case dir of
    DR_UP: begin
      ss := Round((max - cur) / max) * step;
      m_nRy := m_nCurrY + ss;
      if ss = step then begin
        funx := -Round(uny / max * cur);
        if (funx mod 2) <> 0 then
          funx:= funx + 1;
        m_nShiftY:= funx;
      end else begin
        funx := Round(uny / max * (max - cur));
        if (funx mod 2) <> 0 then
          funx:= funx + 1;
        m_nShiftY:= funx;
      end;
    end;
    DR_UPRIGHT: begin
      if max >= 6 then
        v := 2
      else
        v := 0;
      ss := Round((max - cur + v) / max) * step;
      m_nRx := m_nCurrX - ss;
      m_nRy := m_nCurrY + ss;
      if ss = step then begin
        funx := Round(unx / max * cur);
        if (funx mod 2) <> 0 then
          funx:= funx + 1;
        m_nShiftX:= funx;
        funy := -Round(uny / max * cur);
        if (funy mod 2) <> 0 then
          funy:= funy + 1;
        m_nShiftY:= funy;
      end else begin
        funx := -Round(unx / max * (max - cur));
        if (funx mod 2) <> 0 then
          funx:= funx + 1;
        m_nShiftX:= funx;
        funy := Round(uny / max * (max - cur));
        if (funy mod 2) <> 0 then
          funy:= funy + 1;
        m_nShiftY:= funy;
      end;
    end;
    DR_RIGHT: begin
            ss := Round((max - cur) / max) * step;
      m_nRx := m_nCurrX - ss;
      if ss = step then begin
        m_nShiftX := Round(unx / max * cur)
      end else begin
        m_nShiftX := -Round(unx / max * (max - cur));
      end;
      m_nShiftY := 0;
    end;
    DR_DOWNRIGHT: begin
      if max >= 6 then
        v := 2
      else
        v := 0;
      ss := Round((max - cur - v) / max) * step;
      m_nRx := m_nCurrX - ss;
      m_nRy := m_nCurrY - ss;
      if ss = step then begin
        funx := Round(unx / max * cur);
        if (funx mod 2) <> 0 then
          funx:= funx + 1;
        m_nShiftX:= funx;

        funy := Round(uny / max * cur);
        if (funy mod 2) <> 0 then
          funy:= funy + 1;
        m_nShiftY:= funy;
      end else begin
        funx := -Round(unx / max * (max - cur));
        if (funx mod 2) <> 0 then
          funx:= funx + 1;
        m_nShiftX:= funx;
        funy := -Round(uny / max * (max - cur));
        if (funy mod 2) <> 0 then
          funy:= funy + 1;
        m_nShiftY:= funy;
      end;
    end;
    DR_DOWN: begin
      if max >= 6 then
        v := 1
      else
        v := 0;
      ss := Round((max - cur - v) / max) * step;
      m_nShiftX := 0;
      m_nRy := m_nCurrY - ss;
      if ss = step then begin
        funy := Round(uny / max * cur);
        if (funy mod 2) <> 0 then
          funy:= funy + 1;
        m_nShiftY:= funy;
      end else begin
        funy := -Round(uny / max * (max - cur));
        if (funy mod 2) <> 0 then
          funy:= funy + 1;
        m_nShiftY:= funy;
      end;
    end;
    DR_DOWNLEFT:  begin
      if max >= 6 then
        v := 2
      else
        v := 0;
      ss := Round((max - cur - v) / max) * step;
      m_nRx := m_nCurrX + ss;
      m_nRy := m_nCurrY - ss;
      if ss = step then begin
        funx := -Round(unx / max * cur);
        if (funx mod 2) <> 0 then
          funx:= funx + 1;
        m_nShiftX:= funx;
        funy := Round(uny / max * cur);
        if (funy mod 2) <> 0 then
          funy:= funy + 1;
        m_nShiftY:= funy;
      end else begin
        funx := Round(unx / max * (max - cur));
        if (funx mod 2) <> 0 then
          funx:= funx + 1;
        m_nShiftX:= funx;
        funy := -Round(uny / max * (max - cur));
        if (funy mod 2) <> 0 then
          funy:= funy + 1;
        m_nShiftY:= funy;
      end;
    end;
    DR_LEFT: begin
            ss := Round((max - cur) / max) * step;
      m_nRx := m_nCurrX + ss;
      if ss = step then begin
        m_nShiftX := -Round(unx / max * cur)
      end else begin
        m_nShiftX := Round(unx / max * (max - cur));
      end;
      m_nShiftY := 0;
    end;
    DR_UPLEFT: begin
      if max >= 6 then
        v := 2
      else
        v := 0;
      ss := Round((max - cur + v) / max) * step;
      m_nRx := m_nCurrX + ss;
      m_nRy := m_nCurrY + ss;
      if ss = step then begin
        funx := -Round(unx / max * cur);
        if (funx mod 2) <> 0 then
          funx:= funx + 1;
        m_nShiftX:= funx;
        funy := -Round(uny / max * cur);
          if (funy mod 2) <> 0 then
            funy:= funy + 1;
        m_nShiftY:= funy;
      end else begin
        funx := Round(unx / max * (max - cur));
        if (funx mod 2) <> 0 then
          funx:= funx + 1;
        m_nShiftX:= funx;
        funy := Round(uny / max * (max - cur));
        if (funy mod 2) <> 0 then
          funy:= funy + 1;
        m_nShiftY:= funy;
      end;
    end;
  end;
end;

procedure TActor.FeatureChanged;
var
  haircount                 : Integer;
begin
  case m_btRace of
    0: begin
        m_btHair := HAIRfeature(m_nFeature);
        m_btDress := DRESSfeature(m_nFeature);
        m_btSex := m_btDress mod 2;//–ﬁ∏¥∑¢–Õº∞ ±∏ƒ±‰
       // if m_btDress in [24..27] then m_btDress := 18 + m_btSex; //20200719
        m_btWeapon := WEAPONfeature(m_nFeature);
        m_btHorse := Horsefeature(m_nFeatureEx);
        //
        m_btWeaponEffect := m_btHorse;  // div 51;
        m_btHorse := 0;                 // m_btHorse mod 51;

        m_btEffect := Effectfeature(m_nFeatureEx);

        m_nBodyOffset := HUMANFRAME * m_btDress;

        if m_btHair >= 10 then begin
          m_btHairEx := m_btHair div 10;
          m_btHair := m_btHair mod 10;
        end else
          m_btHairEx := 0;

        if m_btHairEx > 0 then begin
          haircount := g_WHair2ImgImages.ImageCount div HUMANFRAME div 2;
          if m_btHairEx > haircount then
            m_btHairEx := haircount;
          m_nHairOffsetEx := HUMANFRAME * ((m_btHairEx - 1) * 2 + m_btSex);
        end else
          m_nHairOffsetEx := -1;

        haircount := g_WHairImgImages.ImageCount div HUMANFRAME div 2;
        if m_btHair > haircount - 1 then
          m_btHair := haircount - 1;

        m_btHair := m_btHair * 2;
        if m_btHair > 1 then
          m_nHairOffset := HUMANFRAME * (m_btHair + m_btSex)
        else
          m_nHairOffset := -1;

        m_nWeaponOffset := HUMANFRAME * m_btWeapon;
        if m_btEffect <> 0 then begin
          if m_btEffect = 50 then
            m_nHumWinOffset := 352
          else
            m_nHumWinOffset := (m_btEffect - 1) * HUMANFRAME;
        end;
      end;
    50: ;                               //npc
  else begin
      m_wAppearance := APPRfeature(m_nFeature);
      m_nBodyOffset := GetOffset(m_wAppearance);
    end;
  end;
end;

function TActor.light: Integer;
begin
  Result := m_nChrLight;
end;

procedure TActor.LoadSurface;
var
  mimg                      : TWMImages;
begin
  if g_gcGeneral[12] then begin
    mimg := GetMonImg(27);
    if mimg <> nil then begin
      if (not m_boReverseFrame) then
        m_BodySurface := mimg.GetCachedImage(GetOffset(27) + m_nCurrentFrame, m_nPx, m_nPy)
      else
        m_BodySurface := mimg.GetCachedImage(GetOffset(27) + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), m_nPx, m_nPy);
    end;
  end else begin
    mimg := GetMonImg(m_wAppearance);
    if mimg <> nil then begin
      if (not m_boReverseFrame) then
        m_BodySurface := mimg.GetCachedImage(GetOffset(m_wAppearance) + m_nCurrentFrame, m_nPx, m_nPy)
      else
        m_BodySurface := mimg.GetCachedImage(GetOffset(m_wAppearance) + m_nEndFrame - (m_nCurrentFrame - m_nStartFrame), m_nPx, m_nPy);
    end;
  end;
end;

function TActor.CharWidth: Integer;
begin
  if m_BodySurface <> nil then
    Result := m_BodySurface.Width
  else Result := 48;
end;

function TActor.CharHeight: Integer;
begin
  if m_BodySurface <> nil then
    Result := m_BodySurface.Height
  else Result := 70;
end;

function TActor.CheckSelect(dx, dy: Integer): Boolean;
var
  c                         : Integer;
begin
  Result := False;
  if m_BodySurface <> nil then begin
    c := m_BodySurface.Pixels(dx, dy);
    if (c <> 0) and
      ((m_BodySurface.Pixels(dx - 1, dy) <> 0) and
      (m_BodySurface.Pixels(dx + 1, dy) <> 0) and
      (m_BodySurface.Pixels(dx, dy - 1) <> 0) and
      (m_BodySurface.Pixels(dx, dy + 1) <> 0)) then
      Result := True;
  end;
end;

procedure TActor.DrawEffSurface(dsurface: TCustomCanvas; source: TCustomLockableTexture; ddx, ddy: Integer; blend: Boolean; ceff: TColorEffect);
var
  cc                        : Integer;
  bLarge                    : Boolean;
  lRect                     : TRect;
  nWidth                    : Integer;
  nHeight                   : Integer;
const
  imgScale                  = 1.6;
begin
//  if HaveStatus(STATE_TRANSPARENT) then
//    blend := True;
//  DrawEffect(ddx, ddy, dsurface, source, ceff, blend);
  cc := 0;                                                              //ASP◊¢ Õ
  try
    if (Source = nil) or (dsurface = nil) then Exit;

    if m_boDeath and g_gcGeneral[8] and not m_boItemExplore and (m_btRace <> 0) then
      Exit;

    if m_fHideMode then
      Exit;

    if m_nState and $00800000 <> 0 then
      blend := True;

//    bLarge := False;
//    if (Source.Width > 300) or (Source.Height > 350) then
//      bLarge := True;

    if not blend then begin
      if ceff = ceNone then begin
        cc := 6;
        dsurface.Draw(ddx, ddy, Source.ClientRect, Source, True);
      end else begin
//        if bLarge then begin
//          cc := 8;
//          DrawEffect(ddx, ddy, dsurface, source, ceff, blend);
//          dsurface.Draw(ddx, ddy, Source.ClientRect, g_ImgLargeMixSurface, True);
//        end else begin
//          cc := 9;
//          g_ImgMixSurface.Draw(0, 0, Source.ClientRect, Source, False);
//          cc := 91;
//          DrawEffect(ddx, ddy, dsurface, source, ceff, blend);
//          cc := 92;
//          dsurface.Draw(ddx, ddy, Source.ClientRect, g_ImgMixSurface, True);
//        end;
        DrawEffect(ddx, ddy, dsurface, source, ceff, blend);
      end;
    end else if ceff = ceNone then begin
      cc := 10;
//      dsurface.DrawBlend(ddx, ddy, Source);
      dsurface.DrawAlpha(ddx, ddy, source, 150{, TBlendingEffect.fxAnti});
    end else begin
//      if bLarge then begin
//        cc := 12;
////        g_ImgLargeMixSurface.Fill(0);
////        g_ImgLargeMixSurface.Draw(0, 0, Source.ClientRect, Source, False);
////        DrawEffect(0, 0, Source.Width, Source.Height, g_ImgLargeMixSurface, ceff);
////        DrawBlend(dsurface, ddx, ddy, g_ImgLargeMixSurface, 0);
//        DrawEffect(ddx, ddy, dsurface, source, ceff, blend);
//      end else begin
//        cc := 13;
////        g_ImgMixSurface.Fill(0);
////        g_ImgMixSurface.Draw(0, 0, Source.ClientRect, Source, False);
////        DrawEffect(0, 0, Source.Width, Source.Height, g_ImgMixSurface, ceff);
////        DrawBlend(dsurface, ddx, ddy, g_ImgMixSurface, 0);
//        DrawEffect(ddx, ddy, dsurface, source, ceff, blend);
//      end;
      DrawEffect(ddx, ddy, dsurface, source, ceff, blend);
    end;

//    if (Self is THumActor) and (m_nState and $00020000 <> 0) and not m_boDeath then begin
////      g_ImgMixSurface.Fill(0);
//      lRect := Source.ClientRect;
//      nWidth := (lRect.Right - lRect.Left);
//      nHeight := (lRect.Bottom - lRect.Top);
//      lRect.Right := lRect.Left + Round(nWidth * imgScale);
//      lRect.Bottom := lRect.Top + Round(nHeight * imgScale);
//
//      dsurface.StretchDraw(lRect, Source.ClientRect, Source, False);
//
//      ddx := ddx - Round(nWidth * ((imgScale - 1) / 2)) + 3;
//      ddy := ddy - Round(nHeight * (imgScale - 1));
//
//      dsurface.DrawBlend(ddx, ddy, source, 0);
//    end;

  except
    on E: Exception do DebugOutStr(Format('TActor.DrawEffSurface (%d) ', [cc]) + E.Message);
  end;
end;

procedure TActor.DrawWeaponGlimmer(dsurface: TCustomCanvas; ddx, ddy: Integer);
var
  idx, ax, ay               : Integer;
//  d                         : TDirectDrawSurface;
begin
  //ªÁøÎæ»«‘..(ø∞»≠∞·) ±◊∑°«» ø¿∑˘...
  (*if BoNextTimeFireHit and WarMode and GlimmingMode then begin
     if GetTickCount - GlimmerTime > 200 then begin
        GlimmerTime := GetTickCount;
        Inc (CurGlimmer);
        if CurGlimmer >= MaxGlimmer then CurGlimmer := 0;
     end;
     idx := GetEffectBase (5-1{ø∞»≠∞·π›¬¶¿”}, 1) + Dir*10 + CurGlimmer;
     d := FrmMain.WMagic.GetCachedImage (idx, ax, ay);
     if d <> nil then
        DrawBlend (dsurface, ddx + ax, ddy + ay, d, 1);
                         //dx + ax + ShiftX,
                         //dy + ay + ShiftY,
                         //d, 1);
  end;*)
end;
//»ÀŒÔœ‘ æ—’…´£¨÷–∂æ

function TActor.GetDrawEffectValue: TColorEffect;
var
  ceff                      : TColorEffect;
begin
  ceff := ceNone;
  if (g_FocusCret = Self) or (g_MagicTarget = Self) then
    ceff := ceBright;
  if m_nState <> 0 then begin
    if m_nState and $80000000 <> 0 then
      ceff := ceGreen;
    if m_nState and $40000000 <> 0 then
      ceff := ceRed;
    if m_nState and $20000000 <> 0 then
      ceff := ceBlue;
    if m_nState and $10000000 <> 0 then
      ceff := ceYellow;
    if m_nState and $08000000 <> 0 then
      ceff := ceFuchsia;
    if m_nState and $04000000 <> 0 then
      ceff := ceGrayScale;
    if m_nState and $02000000 <> 0 then
      ceff := cePurple;
    if m_nState and $01000000 <> 0 then
      ceff := ceWhite;
  end;
  Result := ceff;
end;

procedure TActor.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend: Boolean; boFlag: Boolean; DrawOnSale: Boolean);
var
  bCanDraw                  : Boolean;
  dwTime                    : LongWord;
  idx, ax, ay               : Integer;
  d                         : TCustomLockableTexture;
  ceff                      : TColorEffect;
  wimg                      : TWMImages;
begin
  d := nil;
  if not (m_btDir in [0..7]) then Exit;

  dwTime := GetTickCount;
  if dwTime - m_dwLoadSurfaceTime > (FREE_TEXTURE_TIME div 2) then begin
    m_dwLoadSurfaceTime := dwTime;
    LoadSurface;    // ”…”⁄Õº∆¨√ø60√Îª· Õ∑≈“ª¥Œ£®60√Îƒ⁄Œ¥ π”√µƒª∞£©£¨À˘“‘√ø60√Î“™ºÏ≤È“ªœ¬ «∑Ò“—æ≠±ª Õ∑≈¡À.
  end;

  //if m_sUserName = '' then Exit;  //1015
  if (Self <> g_MySelf) and (Self <> g_MySelf.m_HeroObject) and (m_sUserName = '') and (GetTickCount - m_dwSendQueryUserNameTime > 15 * 1000) then begin
    m_dwSendQueryUserNameTime := GetTickCount;
    frmMain.SendQueryUserName(m_nRecogId, m_nCurrX, m_nCurrY);
  end;

  ceff := GetDrawEffectValue;
  if m_BodySurface <> nil then begin
    DrawEffSurface(dsurface,
      m_BodySurface,
      dx + m_nPx + m_nShiftX,
      dy + m_nPy + m_nShiftY,
      blend,
      ceff);
  end;

  if m_boUseMagic and (m_CurMagic.EffectNumber > 0) then
    if m_nCurEffFrame in [0..m_nSpellFrame - 1] then begin
      GetEffectBase(m_CurMagic.EffectNumber - 1, 0, wimg, idx);
      idx := idx + m_nCurEffFrame;
      if wimg <> nil then
        d := wimg.GetCachedImage(idx, ax, ay);
      if d <> nil then
        dsurface.DrawBlend(
          dx + ax + m_nShiftX,
          dy + ay + m_nShiftY,
          d, 1);
    end;
end;

procedure TActor.DrawEff(dsurface: TCustomCanvas; dx, dy: Integer);
begin
end;

procedure TActor.DrawFocus(dsurface: TCustomCanvas);
var
  Tex                       : TCustomLockableTexture;
  px, py                    : Integer;
  rx, ry                    : Integer;
  FlyX, FlyY                : Integer;
begin
  if GetTickCount - m_dwFocusFrameTick > 100 then begin
    m_dwFocusFrameTick := GetTickCount;
    Inc(m_nCurFocusFrame);
    if m_nCurFocusFrame >= 10 then
      m_nCurFocusFrame := 0;
  end;

  rx := m_nRx;
  ry := m_nRy;
  g_PlayScene.ScreenXYfromMCXY(rx, ry, FlyX, FlyY);
  FlyX := FlyX + m_nShiftX;
  FlyY := FlyY + m_nShiftY;
  Tex := g_WMagic7Images2.GetCachedImage(860 + m_nCurFocusFrame, px, py);
  if Tex <> nil then begin
    dsurface.DrawBlend(
      FlyX + px - UNITX div 2,
      FlyY + py - UNITY div 2,
      Tex,
      1);
  end;
end;

function TActor.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  Result := 0;                          //Jacky
  if g_gcGeneral[12] then pm := GetRaceByPM(18, 27)
  else pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  if m_boDeath then begin
    if m_boSkeleton then
      Result := pm.ActDeath.start
    else
      Result := pm.ActDie.start + m_btDir * (pm.ActDie.frame + pm.ActDie.skip) + (pm.ActDie.frame - 1);
  end else begin
    m_nDefFrameCount := pm.ActStand.frame;
    if m_nCurrentDefFrame < 0 then cf := 0
    else if m_nCurrentDefFrame >= pm.ActStand.frame then cf := 0
    else cf := m_nCurrentDefFrame;
    Result := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip) + cf;
  end;
end;

procedure TActor.DefaultMotion;
begin
  m_boReverseFrame := False;
  if m_boWarMode then begin
    if (GetTickCount - m_dwWarModeTime > 4 * 1000) then //and not BoNextTimeFireHit then
      m_boWarMode := False;
  end;
  m_nCurrentFrame := GetDefaultFrame(m_boWarMode);
  Shift(m_btDir, 0, 1, 1);
end;

procedure TActor.SetSound;
var
  cx, cy, bidx, wunit, attackweapon: Integer;
  hiter                     : TActor;
begin
  if m_btRace = 0 then begin
    if (Self = g_MySelf) and
      ((m_nCurrentAction = SM_WALK) or
      (m_nCurrentAction = SM_BACKSTEP) or
      (m_nCurrentAction = SM_RUN) or
      (m_nCurrentAction = SM_HORSERUN) or
      (m_nCurrentAction = SM_RUSH) or
      (m_nCurrentAction = SM_RUSHKUNG)) then begin
      cx := g_MySelf.m_nCurrX - Map.m_nBlockLeft;
      cy := g_MySelf.m_nCurrY - Map.m_nBlockTop;
      cx := cx div 2 * 2;
      cy := cy div 2 * 2;
      bidx := Map.m_MArr[cx, cy].wBkImg and $7FFF;
      wunit := Map.m_MArr[cx, cy].btArea;
      bidx := wunit * 10000 + bidx - 1;
      case bidx of
        330..349, 450..454, 550..554, 750..754, 950..954, 1250..1254, 1400..1424, 1455..1474, 1500..1524, 1550..1574:
          m_nFootStepSound := s_walk_lawn_l;

        250..254, 1005..1009, 1050..1054, 1060..1064, 1450..1454, 1650..1654:
          m_nFootStepSound := s_walk_rough_l;

        605..609, 650..654, 660..664, 2000..2049, 3025..3049, 2400..2424, 4625..4649, 4675..4678:
          m_nFootStepSound := s_walk_stone_l;

        1825..1924, 2150..2174, 3075..3099, 3325..3349, 3375..3399:
          m_nFootStepSound := s_walk_cave_l;

        3230, 3231, 3246, 3277:
          m_nFootStepSound := s_walk_wood_l;

        3780..3799:
          m_nFootStepSound := s_walk_wood_l;

        3825..4434:
          if (bidx - 3825) mod 25 = 0 then m_nFootStepSound := s_walk_wood_l
          else m_nFootStepSound := s_walk_ground_l;

        2075..2099, 2125..2149:
          m_nFootStepSound := s_walk_room_l;

        1800..1824:
          m_nFootStepSound := s_walk_water_l;

      else
        m_nFootStepSound := s_walk_ground_l;
      end;

      if (bidx >= 825) and (bidx <= 1349) then begin
        if ((bidx - 825) div 25) mod 2 = 0 then
          m_nFootStepSound := s_walk_stone_l;
      end;
      if (bidx >= 1375) and (bidx <= 1799) then begin
        if ((bidx - 1375) div 25) mod 2 = 0 then
          m_nFootStepSound := s_walk_cave_l;
      end;
      case bidx of
        1385, 1386, 1391, 1392:
          m_nFootStepSound := s_walk_wood_l;
      end;

      bidx := Map.m_MArr[cx, cy].wMidImg and $7FFF;
      bidx := bidx - 1;
      case bidx of
        0..115:
          m_nFootStepSound := s_walk_ground_l;
        120..124:
          m_nFootStepSound := s_walk_lawn_l;
      end;

      bidx := Map.m_MArr[cx, cy].wFrImg and $7FFF;
      bidx := bidx - 1;
      case bidx of
        221..289, 583..658, 1183..1206, 7163..7295, 7404..7414:
          m_nFootStepSound := s_walk_stone_l;
        3125..3267, {3319..3345, 3376..3433,} 3757..3948, 6030..6999:
          m_nFootStepSound := s_walk_wood_l;
        3316..3589:
          m_nFootStepSound := s_walk_room_l;
      end;
      if (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) then
        m_nFootStepSound := m_nFootStepSound + 2;
    end;

    if m_btSex = 0 then begin
      m_nScreamSound := s_man_struck;
      m_nDieSound := s_man_die;
    end else begin
      m_nScreamSound := s_wom_struck;
      m_nDieSound := s_wom_die;
    end;

    case m_nCurrentAction of
      SM_THROW,
        SM_HIT,
        SM_HIT + 1,
        SM_HIT + 2,
        SM_POWERHIT,
        SM_LONGHIT,
        SM_HERO_LONGHIT,
        SM_HERO_LONGHIT2,
        SM_SQUHIT,
        SM_CRSHIT,
        SM_TWNHIT,
        SM_WIDEHIT,
        SM_FIREHIT,
        SM_SMITEHIT,
        SM_PURSUEHIT,
        SM_SMITELONGHIT,
        SM_SMITELONGHIT2,
        SM_SMITELONGHIT3,
        SM_SMITEWIDEHIT,
        SM_SMITEWIDEHIT2: begin
          case (m_btWeapon div 2) of
            6, 20: m_nWeaponSound := s_hit_short;
            1, 27, 28, 33: m_nWeaponSound := s_hit_wooden;
            2, 13, 9, 5, 14, 22, 25, 30, 35, 36, 37: m_nWeaponSound := s_hit_sword;
            4, 17, 10, 15, 16, 23, 26, 29, 31, 34: m_nWeaponSound := s_hit_do;
            3, 7, 11: m_nWeaponSound := s_hit_axe;
            24: m_nWeaponSound := s_hit_club;
            8, 12, 18, 21, 32: m_nWeaponSound := s_hit_long;
          else
            m_nWeaponSound := s_hit_fist;
          end;
        end;
      SM_WWJATTACK: m_nWeaponSound := 122;
      SM_WSJATTACK: m_nWeaponSound := 123;
      SM_WTJATTACK: m_nWeaponSound := 124;
      SM_STRUCK: begin
          if m_nMagicStruckSound >= 1 then begin
          end else begin
            hiter := g_PlayScene.FindActor(m_nHiterCode);
            attackweapon := 0;
            if hiter <> nil then begin
              attackweapon := hiter.m_btWeapon div 2;
              if hiter.m_btRace = 0 then begin
                case (m_btDress div 2) of
                  3:
                    case attackweapon of
                      6: m_nStruckSound := s_struck_armor_sword;
                      1, 2, 4, 5, 9, 10, 13, 14, 15, 16, 17: m_nStruckSound := s_struck_armor_sword;
                      3, 7, 11: m_nStruckSound := s_struck_armor_axe;
                      8, 12, 18: m_nStruckSound := s_struck_armor_longstick;
                    else m_nStruckSound := s_struck_armor_fist;
                    end;
                else
                  case attackweapon of
                    6: m_nStruckSound := s_struck_body_sword;
                    1, 2, 4, 5, 9, 10, 13, 14, 15, 16, 17: m_nStruckSound := s_struck_body_sword;
                    3, 7, 11: m_nStruckSound := s_struck_body_axe;
                    8, 12, 18: m_nStruckSound := s_struck_body_longstick;
                  else m_nStruckSound := s_struck_body_fist;
                  end;
                end;

              end;
            end;
          end;
        end;
    end;

    if m_boUseMagic and (m_CurMagic.MagicSerial > 0) then begin
      m_nMagicStartSound := 10000 + m_CurMagic.MagicSerial * 10;
      m_nMagicFireSound := m_nMagicStartSound + 1;
      m_nMagicExplosionSound := m_nMagicStartSound + 2;
    end;

  end else begin
    if m_nCurrentAction = SM_STRUCK then begin
      if m_nMagicStruckSound >= 1 then begin
        //strucksound := s_struck_magic;
      end else begin
        hiter := g_PlayScene.FindActor(m_nHiterCode);
        if hiter <> nil then begin
          attackweapon := hiter.m_btWeapon div 2;
          case attackweapon of
            6: m_nStruckSound := s_struck_body_sword;
            1, 2, 4, 5, 9, 10, 13, 14, 15, 16, 17: m_nStruckSound := s_struck_body_sword;
            3, 11: m_nStruckSound := s_struck_body_axe;
            8, 12, 18: m_nStruckSound := s_struck_body_longstick;
          else m_nStruckSound := s_struck_body_fist;
          end;
        end;
      end;
    end;

    if m_btRace = 50 then begin
      //
    end else begin
      if (m_wAppearance >= 700) and (m_wAppearance <= 702) then begin
        m_nAppearSound := 200 + (37) * 10;
        m_nNormalSound := 200 + (37) * 10 + 1;
        m_nAttackSound := 200 + (37) * 10 + 2;
        m_nWeaponSound := 200 + (37) * 10 + 3;
        m_nScreamSound := 200 + (37) * 10 + 4;
        m_nDieSound := 200 + (37) * 10 + 5;
        m_nDie2Sound := 200 + (37) * 10 + 6;
      end else if (m_wAppearance = 703) or (m_wAppearance = 705) or (m_wAppearance = 707) then begin
        m_nAppearSound := 200 + (170) * 10;
        m_nNormalSound := 200 + (170) * 10 + 1;
        m_nAttackSound := 200 + (170) * 10 + 2;
        m_nWeaponSound := 200 + (170) * 10 + 3;
        m_nScreamSound := 200 + (170) * 10 + 4;
        m_nDieSound := 200 + (170) * 10 + 5;
        m_nDie2Sound := 200 + (170) * 10 + 6;
      end else if (m_wAppearance = 704) or (m_wAppearance = 706) or (m_wAppearance = 708) then begin
        m_nAppearSound := 200 + (171) * 10;
        m_nNormalSound := 200 + (171) * 10 + 1;
        m_nAttackSound := 200 + (171) * 10 + 2;
        m_nWeaponSound := 200 + (171) * 10 + 3;
        m_nScreamSound := 200 + (171) * 10 + 4;
        m_nDieSound := 200 + (171) * 10 + 5;
        m_nDie2Sound := 200 + (171) * 10 + 6;
      end else begin
        m_nAppearSound := 200 + (m_wAppearance) * 10;
        m_nNormalSound := 200 + (m_wAppearance) * 10 + 1;
        m_nAttackSound := 200 + (m_wAppearance) * 10 + 2;
        m_nWeaponSound := 200 + (m_wAppearance) * 10 + 3;
        m_nScreamSound := 200 + (m_wAppearance) * 10 + 4;
        m_nDieSound := 200 + (m_wAppearance) * 10 + 5;
        m_nDie2Sound := 200 + (m_wAppearance) * 10 + 6;
      end;
    end;
  end;

  if m_nCurrentAction = SM_STRUCK then begin
    hiter := g_PlayScene.FindActor(m_nHiterCode);
    attackweapon := 0;
    if hiter <> nil then begin
      attackweapon := hiter.m_btWeapon div 2;
      if hiter.m_btRace = 0 then
        case (attackweapon div 2) of
          6, 20: m_nStruckWeaponSound := s_struck_short;
          1: m_nStruckWeaponSound := s_struck_wooden;
          2, 13, 9, 5, 14, 22: m_nStruckWeaponSound := s_struck_sword;
          4, 17, 10, 15, 16, 23: m_nStruckWeaponSound := s_struck_do;
          3, 7, 11: m_nStruckWeaponSound := s_struck_axe;
          24: m_nStruckWeaponSound := s_struck_club;
          8, 12, 18, 21: m_nStruckWeaponSound := s_struck_wooden;
          //else struckweaponsound := s_struck_fist;
        end;
    end;
  end;
end;

procedure TActor.RunSound;
begin
  m_boRunSound := True;
  SetSound;
  case m_nCurrentAction of
    SM_STRUCK: begin
        if (m_nStruckWeaponSound >= 0) then g_SndMgr.PlaySound(m_nStruckWeaponSound, m_nCurrX, m_nCurrY);
        if (m_nStruckSound >= 0) then g_SndMgr.PlaySound(m_nStruckSound, m_nCurrX, m_nCurrY);
        if (m_nScreamSound >= 0) then g_SndMgr.PlaySound(m_nScreamSound, m_nCurrX, m_nCurrY);
      end;
    SM_NOWDEATH: if (m_nDieSound >= 0) then begin
        g_SndMgr.PlaySound(m_nDieSound, m_nCurrX, m_nCurrY);
        if Self = g_MySelf then
          g_SndMgr.PlayBKGSound(bmg_gameover);
      end;
    SM_THROW, SM_HIT, SM_FLYAXE, SM_LIGHTING, SM_LIGHTING_1..SM_LIGHTING_3, SM_DIGDOWN: if m_nAttackSound >= 0 then
        g_SndMgr.PlaySound(m_nAttackSound, m_nCurrX, m_nCurrY);
    SM_ALIVE, SM_DIGUP: begin
        g_SndMgr.SilenceSound;
        g_SndMgr.PlaySound(m_nAppearSound, m_nCurrX, m_nCurrY);
      end;
    SM_SPELL: g_SndMgr.PlaySound(m_nMagicStartSound, m_nCurrX, m_nCurrY);
  end;
end;

procedure TActor.RunActSound(frame: Integer);
begin
  if m_boRunSound then begin
    if m_btRace = 0 then begin
      case m_nCurrentAction of
        SM_THROW, SM_HIT, SM_HIT + 1, SM_HIT + 2:
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_POWERHIT:
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            if m_btSex = 0 then
              g_SndMgr.PlaySound(s_yedo_man, m_nCurrX, m_nCurrY)
            else
              g_SndMgr.PlaySound(s_yedo_woman, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_LONGHIT:
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            g_SndMgr.PlaySound(s_longhit, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_HERO_LONGHIT, SM_HERO_LONGHIT2:
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            g_SndMgr.PlaySound('Wav\longsword-hit.wav', m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_SQUHIT:
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            g_SndMgr.PlaySound('Wav\squarehit.wav', m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_WIDEHIT:
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            g_SndMgr.PlaySound(s_widehit, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_FIREHIT:
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            //
            if m_CurMagic.magfirelv > MAXMAGICLV then
              g_SndMgr.PlaySound('Wav\M34-1.wav', m_nCurrX, m_nCurrY)
            else
              g_SndMgr.PlaySound(s_firehit, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_PURSUEHIT:
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            g_SndMgr.PlaySound(180 + m_btSex, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_SMITEHIT:
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            if m_btSex = 0 then
              g_SndMgr.PlaySound('Wav\cboZs1_start_m.wav', m_nCurrX, m_nCurrY)
            else
              g_SndMgr.PlaySound('Wav\cboZs1_start_w.wav', m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_SMITELONGHIT: if m_boSmiteLongHit = 1 then begin
            if frame = 2 then begin
              g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
              if m_btSex = 0 then
                g_SndMgr.PlaySound('Wav\cboZs3_start_m.wav', m_nCurrX, m_nCurrY)
              else
                g_SndMgr.PlaySound('Wav\cboZs3_start_w.wav', m_nCurrX, m_nCurrY);
              m_boRunSound := False;
            end;
          end;
        SM_SMITELONGHIT3: begin
            if frame = 2 then begin
              g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
              if m_btSex = 0 then
                g_SndMgr.PlaySound('Wav\cboZs3_start_m.wav', m_nCurrX, m_nCurrY)
              else
                g_SndMgr.PlaySound('Wav\cboZs3_start_w.wav', m_nCurrX, m_nCurrY);
              m_boRunSound := False;
            end;
          end;
        SM_SMITELONGHIT2, SM_SMITEWIDEHIT2: begin
            if frame = 2 then begin
              g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
              if m_btSex = 0 then
                g_SndMgr.PlaySound('Wav\cboZs3_start_m.wav', m_nCurrX, m_nCurrY)
              else
                g_SndMgr.PlaySound('Wav\cboZs3_start_w.wav', m_nCurrX, m_nCurrY);
              m_boRunSound := False;
            end;
          end;
        SM_SMITEWIDEHIT: begin
            if frame = 2 then begin
              g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
              g_SndMgr.PlaySound('Wav\cboZs4_start.wav', m_nCurrX, m_nCurrY);
              m_boRunSound := False;
            end;
          end;

        SM_RUSHEX: if frame = 1 then begin
            g_SndMgr.PlaySound('Wav\cboZs2_start.wav', m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_CRSHIT:
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            g_SndMgr.PlaySound(s_crshit, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_TWNHIT:
          if frame = 1 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            g_SndMgr.PlaySound(s_twinhit, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        SM_WWJATTACK: begin
            g_SndMgr.PlaySound(122, m_nCurrX, m_nCurrY);
            if frame = 2 then g_SndMgr.PlaySound(10512, m_nCurrX, m_nCurrY);
            if frame > 4 then m_boRunSound := False;
          end;
        SM_WSJATTACK: begin
            if frame = 2 then begin
              g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
              g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            end;
          end;
        SM_WTJATTACK: begin
            if frame = 2 then begin
              g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
              g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            end;
          end;
        SM_SPELL: {if self = g_myself then} begin
            case m_CurMagic.EffectNumber of
              61: begin                 //≈¸–«’∂
                  g_SndMgr.PlaySound(10510, m_nCurrX, m_nCurrY); //10
                  if frame > 2 then m_boRunSound := False;
                end;
              62: begin                 //¿◊ˆ™“ªª˜
                  g_SndMgr.PlaySound(10520, m_nCurrX, m_nCurrY);
                  if frame > 2 then m_boRunSound := False;
                end;
              63: begin                 // …ªÍ’”‘Û
                  g_SndMgr.PlaySound(10530, m_nCurrX, m_nCurrY);
                  if frame > 2 then m_boRunSound := False;
                end;
              64: begin                 //ƒ©»’…Û≈–
                  g_SndMgr.PlaySound(10540, m_nCurrX, m_nCurrY);
                  if frame > 2 then m_boRunSound := False;
                end;
              65: begin                 //ª¡˙∆¯—Ê
                  g_SndMgr.PlaySound(10550, m_nCurrX, m_nCurrY);
                  if frame > 2 then m_boRunSound := False;
                end;

            end;
          end;
      end;
    end else begin
      if m_btRace = 50 then begin
        //
      end else begin
        if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_TURN) then begin
          if (frame = 1) and (Random(8) = 1) then begin
            g_SndMgr.PlaySound(m_nNormalSound, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        end else if m_nCurrentAction = SM_HIT then begin
          if (frame = 3) and (m_nAttackSound >= 0) then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
        end else if m_nCurrentAction = SM_POWERHIT then begin
          if frame = 2 then begin
            g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
            g_SndMgr.PlaySound(s_yedo_man, m_nCurrX, m_nCurrY);
            m_boRunSound := False;
          end;
          {end else if m_nCurrentAction = SM_LIGHTING then begin
            case m_wAppearance of
              251: begin
                  if frame = 2 then begin
                    g_SndMgr.PlaySound(m_nWeaponSound, m_nCurrX, m_nCurrY);
                    g_SndMgr.PlaySound(m_nDie2Sound, m_nCurrX, m_nCurrY);
                    m_boRunSound := False;
                  end;
                end;

            end;}
        end;

        case m_wAppearance of
          80: begin
              if m_nCurrentAction = SM_NOWDEATH then begin
                if (frame = 2) then begin
                  g_SndMgr.PlaySound(m_nDie2Sound, m_nCurrX, m_nCurrY);
                  m_boRunSound := False;
                end;
              end;
            end;
        end;
      end;

    end;
  end;
end;

procedure TActor.RunFrameAction(frame: Integer);
begin
end;

procedure TActor.ActionEnded();
begin
end;

procedure TActor.ReadyNextAction();
begin
end;

procedure TActor.Run;

  function MagicTimeOut: Boolean;
  begin
    if Self = g_MySelf then begin
      Result := GetTickCount - m_dwWaitMagicRequest > 1800;
    end else begin
      Result := GetTickCount - m_dwWaitMagicRequest > 900;
    end;
    if Result then
      m_CurMagic.ServerMagicCode := 0;
  end;

var
  prv, mx, my               : Integer;
  dwFrameTimetime           : LongWord;
  boFly                     : Boolean;
begin
  if (m_nCurrentAction = SM_WALK) or
    (m_nCurrentAction = SM_BACKSTEP) or
    (m_nCurrentAction = SM_RUN) or
    (m_nCurrentAction = SM_HORSERUN) or
    (m_nCurrentAction = SM_RUSH) or
    (m_nCurrentAction = SM_RUSHEX) or
    (m_nCurrentAction = SM_RUSHKUNG) then Exit;

  m_boMsgMuch := False;
  if Self <> g_MySelf then
    if m_MsgList.count >= 2 then
      m_boMsgMuch := True;

  RunActSound(m_nCurrentFrame - m_nStartFrame);
  RunFrameAction(m_nCurrentFrame - m_nStartFrame);

  prv := m_nCurrentFrame;
  if m_nCurrentAction <> 0 then begin
    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame;

    if (Self <> g_MySelf) and (m_boUseMagic) then
      dwFrameTimetime := Round(m_dwFrameTime / 1.8) //1.4
    else if m_boMsgMuch then
      dwFrameTimetime := Round(m_dwFrameTime * 2 / 3) //Round(m_dwFrameTime / 1.6)
    else
      dwFrameTimetime := m_dwFrameTime;

    if GetTickCount - m_dwStartTime > dwFrameTimetime then begin
      if m_nCurrentFrame < m_nEndFrame then begin
        if m_boUseMagic then begin
          if (m_nCurEffFrame = m_nSpellFrame - 2) or (MagicTimeOut) then begin
            if (m_CurMagic.ServerMagicCode >= 0) or (MagicTimeOut) then begin
              Inc(m_nCurrentFrame);
              Inc(m_nCurEffFrame);
              m_dwStartTime := GetTickCount;
            end;
          end else begin
            if m_nCurrentFrame < m_nEndFrame - 1 then Inc(m_nCurrentFrame);
            Inc(m_nCurEffFrame);
            m_dwStartTime := GetTickCount;
          end;
        end else begin
          Inc(m_nCurrentFrame);
          m_dwStartTime := GetTickCount;
        end;

      end else begin
        if m_boDelActionAfterFinished then
          m_boDelActor := True;
        if Self = g_MySelf then begin
          if frmMain.ServerAcceptNextAction then begin
            ActionEnded;
            m_nCurrentAction := 0;
            m_boUseMagic := False;
            if m_btRace <> 50 then begin
              m_boUseEffect := False;
              m_boHitEffect := False;
            end;
          end;
        end else begin
          ActionEnded;
          m_nCurrentAction := 0;
          m_boUseMagic := False;
          if m_btRace <> 50 then begin
            m_boUseEffect := False;
            m_boHitEffect := False;
          end;
        end;
      end;

      if m_boUseMagic then begin
        if m_nCurEffFrame = m_nSpellFrame - 1 then begin
          if m_CurMagic.ServerMagicCode > 0 then begin
            with m_CurMagic do begin
              g_PlayScene.NewMagic(Self,
                ServerMagicCode,
                EffectNumber,
                m_nCurrX,
                m_nCurrY,
                targx,
                targy,
                target,
                EffectType,
                Recusion,
                anitime,
                boFly,
                magfirelv);
              if boFly then begin

                g_SndMgr.PlaySound(m_nMagicFireSound, m_nCurrX, m_nCurrY)
              end else begin
                g_SndMgr.PlaySound(m_nMagicExplosionSound, targx, targy);
              end;
            end;
          end;
          m_CurMagic.ServerMagicCode := 0;
        end;
      end;
    end;
    if m_wAppearance in [0, 1, 43] then
      m_nCurrentDefFrame := -10
    else
      m_nCurrentDefFrame := 0;
    m_dwDefFrameTime := GetTickCount;
  end else if Integer(GetTickCount - m_dwSmoothMoveTime) > 200 then begin
    if GetTickCount - m_dwDefFrameTime > 500 then begin
      m_dwDefFrameTime := GetTickCount;
      Inc(m_nCurrentDefFrame);
      if m_nCurrentDefFrame >= m_nDefFrameCount then
        m_nCurrentDefFrame := 0;
    end;
    DefaultMotion;
  end;

  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;

end;

function TActor.Move({step: Integer}): Boolean;
var
  prv, curstep, maxstep     : Integer;
  fastmove, normmove        : Boolean;
begin
  Result := False;

  fastmove := False;
  normmove := False;
  if (m_nCurrentAction = SM_BACKSTEP) then
    fastmove := True;
  if (m_nCurrentAction = SM_RUSH) or (m_nCurrentAction = SM_RUSHEX) or (m_nCurrentAction = SM_RUSHKUNG) then
    normmove := True;

  if not fastmove and not normmove then begin

    m_boMoveSlow := False;              //g_boMoveSlow := False;
    m_boAttackSlow := False;            //g_boAttackSlow := False;
    m_nMoveSlowLevel := 0;              //g_nMoveSlowLevel := 0;

    if m_nState and $10000000 <> 0 then begin
      m_nMoveSlowLevel := 1;
      m_boMoveSlow := True;
    end;

    if (m_btRace = 0) and (Self = g_MySelf) then begin
      {if m_Abil.Weight > m_Abil.MaxWeight then begin
        m_nMoveSlowLevel := m_Abil.Weight div m_Abil.MaxWeight;
        m_boMoveSlow := True;
      end;

      if m_Abil.WearWeight > m_Abil.MaxWearWeight then begin
        m_nMoveSlowLevel := m_nMoveSlowLevel + m_Abil.WearWeight div m_Abil.MaxWearWeight;
        m_boMoveSlow := True;
      end;

      if m_Abil.HandWeight > m_Abil.MaxHandWeight then begin
        m_boAttackSlow := True;
      end;}
    end;

    if m_boMoveSlow and (m_nSkipTick < m_nMoveSlowLevel) then begin
      Inc(m_nSkipTick);
      Exit;
    end else
      m_nSkipTick := 0;

    if (m_btRace = 0) and (Self = g_MySelf) then begin
      if (m_nCurrentAction in [5..7, 9, 11, 13]) then begin
        case (m_nCurrentFrame - m_nStartFrame) of
          1: g_SndMgr.PlaySound(m_nFootStepSound);
          4: g_SndMgr.PlaySound(m_nFootStepSound + 1);
        end;
      end;
    end;
  end;

  Result := False;
  m_boMsgMuch := (Self <> g_MySelf) and (m_MsgList.count >= 2);

  prv := m_nCurrentFrame;
  if (m_nCurrentAction = SM_WALK) or (m_nCurrentAction = SM_RUN) or (m_nCurrentAction = SM_HORSERUN) or
    (m_nCurrentAction = SM_RUSH) or (m_nCurrentAction = SM_RUSHKUNG) then begin

    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame - 1;

    if m_nCurrentFrame < m_nEndFrame then begin
      Inc(m_nCurrentFrame);
      if m_boMsgMuch and not normmove then
        if m_nCurrentFrame < m_nEndFrame then
          Inc(m_nCurrentFrame);
      curstep := m_nCurrentFrame - m_nStartFrame + 1;
      maxstep := m_nEndFrame - m_nStartFrame + 1;
      Shift(m_btDir, m_nMoveStep, curstep, maxstep);
    end;

    if m_nCurrentFrame >= m_nEndFrame then begin
      if Self = g_MySelf then begin
        if frmMain.ServerAcceptNextAction then begin
          m_nCurrentAction := 0;
          m_boLockEndFrame := True;
          m_dwSmoothMoveTime := GetTickCount;
          m_boUseCboLib := False;
        end;
      end else begin
        m_nCurrentAction := 0;
        m_boLockEndFrame := True;
        m_dwSmoothMoveTime := GetTickCount;
        m_boUseCboLib := False;
      end;
    end;
    if (m_nCurrentAction = SM_RUSH) or (m_nCurrentAction = SM_RUSHEX) then begin
      if Self = g_MySelf then begin
        g_dwDizzyDelayStart := GetTickCount;
        g_dwDizzyDelayTime := 300;
      end;
    end;
    if m_nCurrentAction = SM_RUSHKUNG then begin
      if m_nCurrentFrame >= m_nEndFrame - 3 then begin
        m_nCurrX := m_nActBeforeX;
        m_nCurrY := m_nActBeforeY;
        m_nRx := m_nCurrX;
        m_nRy := m_nCurrY;
        m_nCurrentAction := 0;
        m_boUseCboLib := False;
        m_boLockEndFrame := True;
      end;
    end;
    Result := True;
  end;

  if (m_nCurrentAction = SM_RUSHEX) then begin

    if (m_nCurrentFrame < m_nStartFrame) or (m_nCurrentFrame > m_nEndFrame) then
      m_nCurrentFrame := m_nStartFrame - 1;

    if m_nCurrentFrame < m_nEndFrame then begin
      Inc(m_nCurrentFrame);
      if m_boMsgMuch and not normmove then
        if m_nCurrentFrame < m_nEndFrame then
          Inc(m_nCurrentFrame);
      curstep := m_nCurrentFrame - m_nStartFrame + 1;
      maxstep := m_nEndFrame - m_nStartFrame + 1;
      Shift(m_btDir, m_nMoveStep, curstep, maxstep);

      ReadyNextAction();
      frmMain.LastHitTick := GetTickCount;

    end else if m_nCurrentFrame >= m_nEndFrame then begin
      if Self = g_MySelf then begin
        //if frmMain.ServerAcceptNextAction then begin
        ActionEnded();
        m_nCurrentAction := 0;
        m_boLockEndFrame := True;
        m_dwSmoothMoveTime := GetTickCount - 200;
        m_boUseCboLib := False;
        //end;
      end else begin
        ActionEnded();
        m_nCurrentAction := 0;
        m_boLockEndFrame := True;
        m_dwSmoothMoveTime := GetTickCount - 200;
        m_boUseCboLib := False;
      end;
      frmMain.LastHitTick := 0;
    end;

    if Self = g_MySelf then begin
      g_dwDizzyDelayStart := GetTickCount;
      g_dwDizzyDelayTime := 300;
    end;
    Result := True;
  end;

  if (m_nCurrentAction = SM_BACKSTEP) then begin
    if (m_nCurrentFrame > m_nEndFrame) or (m_nCurrentFrame < m_nStartFrame) then
      m_nCurrentFrame := m_nEndFrame + 1;
    if m_nCurrentFrame > m_nStartFrame then begin
      Dec(m_nCurrentFrame);
      if m_boMsgMuch or fastmove then
        if m_nCurrentFrame > m_nStartFrame then
          Dec(m_nCurrentFrame);
      curstep := m_nEndFrame - m_nCurrentFrame + 1;
      maxstep := m_nEndFrame - m_nStartFrame + 1;
      Shift(GetBack(m_btDir), m_nMoveStep, curstep, maxstep);
    end;
    if m_nCurrentFrame <= m_nStartFrame then begin
      if Self = g_MySelf then begin
        m_nCurrentAction := 0;
        m_boLockEndFrame := True;
        m_dwSmoothMoveTime := GetTickCount;
        g_dwDizzyDelayStart := GetTickCount;
        g_dwDizzyDelayTime := 1000;
      end else begin
        m_nCurrentAction := 0;
        m_boLockEndFrame := True;
        m_dwSmoothMoveTime := GetTickCount;
      end;
    end;
    Result := True;
  end;
  if prv <> m_nCurrentFrame then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
end;

procedure TActor.MoveFail;
begin
  m_nCurrentAction := 0;
  m_boLockEndFrame := True;
  g_MySelf.m_nCurrX := m_nOldx;
  g_MySelf.m_nCurrY := m_nOldy;
  g_MySelf.m_btDir := m_nOldDir;
  CleanUserMsgs;
end;

function TActor.CanCancelAction: Boolean;
begin
  Result := False;
  if m_nCurrentAction = SM_HIT then
    if not m_boUseEffect then
      Result := True;
end;

procedure TActor.CancelAction;
begin
  m_nCurrentAction := 0;
  m_boLockEndFrame := True;
end;

procedure TActor.CleanCharMapSetting(X, Y: Integer);
begin
  g_MySelf.m_nCurrX := X;
  g_MySelf.m_nCurrY := Y;
  g_MySelf.m_nRx := X;
  g_MySelf.m_nRy := Y;
  m_nOldx := X;
  m_nOldy := Y;
  m_nCurrentAction := 0;
  m_nCurrentFrame := -1;
  CleanUserMsgs;
end;

procedure TActor.StruckShowDamage(Str: string);
var
  idx                       : Integer;
begin
  idx := 0;
  m_StruckDamage.AddObject(Str, TObject(idx));
end;

procedure TActor.StruckShowDamage2(Str: string);
var
  idx                       : Integer;
begin
  idx := 0;
  m_StruckDamage2.AddObject(Str, TObject(idx));
end;

procedure TActor.Say(Str: string);
var
  i, Len, aline, n          : Integer;
  dline, temp               : string;
  loop                      : Boolean;
const
  MAXWIDTH                  = 200;
begin
  m_dwSayTime := GetTickCount;
  m_nSayLineCount := 0;
  n := 0;
  loop := True;
  while loop do begin
    temp := '';
    i := 1;
    Len := Length(Str);
    while True do begin
      if i > Len then begin
        loop := False;
        Break;
      end;
      if byte(Str[i]) >= 128 then begin
        temp := temp + Str[i];
        Inc(i);
        if i <= Len then
          temp := temp + Str[i]
        else begin
          loop := False;
          Break;
        end;
      end else
        temp := temp + Str[i];
      aline := FontManager.Default.TextWidth(temp);
      if aline > MAXWIDTH then begin
        m_SayingArr[n] := temp;
        m_SayWidthsArr[n] := aline;
        Inc(m_nSayLineCount);
        Inc(n);
        if n >= MAXSAY then begin
          loop := False;
          Break;
        end;
        Str := Copy(Str, i + 1, Len - i);
        temp := '';
        Break;
      end;
      Inc(i);
    end;
    if temp <> '' then begin
      if n < MAXWIDTH then begin
        m_SayingArr[n] := temp;
        m_SayWidthsArr[n] := FontManager.Default.TextWidth(temp);
        Inc(m_nSayLineCount);
      end;
    end;
  end;
end;

procedure TNpcActor.CalcActorFrame;
var
  pm                        : pTMonsterAction;
  haircount                 : Integer;
begin
  m_boUseMagic := False;
  m_boNewMagic := False;
  m_boUseCboLib := False;
  m_nCurrentFrame := -1;
  m_nBodyOffset := GetNpcOffset(m_wAppearance);
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  m_btDir := m_btDir mod 3;
  case m_nCurrentAction of
    SM_TURN: begin
        case m_wAppearance of
          54..58, 112..117: begin

            end;
          59, 70..75, 81..85, 90..92, 94..98, 118..123, 130, 131, 132: begin
              m_nStartFrame := pm.ActStand.start;
              m_nEndFrame := m_nStartFrame + pm.ActStand.frame - 1;
              m_dwFrameTime := pm.ActStand.ftime;
              m_dwStartTime := GetTickCount;
              m_nDefFrameCount := pm.ActStand.frame;
              Shift(m_btDir, 0, 0, 1);
            end;
        else begin
            m_nStartFrame := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip);
            m_nEndFrame := m_nStartFrame + pm.ActStand.frame - 1;
            m_dwFrameTime := pm.ActStand.ftime;
            m_dwStartTime := GetTickCount;
            m_nDefFrameCount := pm.ActStand.frame;
            Shift(m_btDir, 0, 0, 1);
          end;
        end;
        if not m_boUseEffect then begin
          if (m_wAppearance in [33, 34]) then begin
            m_boUseEffect := True;
            m_nEffectFrame := m_nEffectStart;
            m_nEffectEnd := m_nEffectStart + 9;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 300;
          end else if m_wAppearance in [54..58, 94..98] then begin
            //m_nStartFrame := 0;
            //m_nEndFrame := 0;
            m_boUseEffect := True;
            m_nEffectStart := 0;
            m_nEffectEnd := 8;
            m_nEffectFrame := 0;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 150;
          end else if m_wAppearance in [42..47] then begin
            m_nStartFrame := 20;
            m_nEndFrame := 10;
            m_boUseEffect := True;
            m_nEffectStart := 0;
            m_nEffectFrame := 0;
            m_nEffectEnd := 19;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 100;

          end else if m_wAppearance in [118..120] then begin
            m_boUseEffect := True;
            m_nEffectStart := 10;
            m_nEffectEnd := 10 + 16 - 1;
            m_nEffectFrame := 16;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 200;
          end else if m_wAppearance in [122..123] then begin
            m_boUseEffect := True;
            m_nEffectStart := 20;
            m_nEffectEnd := 20 + 9 - 1;
            m_nEffectFrame := 9;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 200;

          end else if m_wAppearance = 131 then begin
            m_boUseEffect := True;
            m_nEffectStart := 10;
            m_nEffectEnd := 21;
            m_nEffectFrame := 12;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 100;
          end else if m_wAppearance = 132 then begin
            m_boUseEffect := True;
            m_nEffectStart := 20;
            m_nEffectEnd := 39;
            m_nEffectFrame := 20;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 100;
          end else if m_wAppearance = 51 then begin
            m_boUseEffect := True;
            m_nEffectStart := 60;
            m_nEffectFrame := m_nEffectStart;
            m_nEffectEnd := m_nEffectStart + 7;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 150;
          end else if m_wAppearance in [60..67] then begin
            m_boUseEffect := True;
            m_nEffectStart := 0;
            m_nEffectFrame := m_nEffectStart;
            m_nEffectEnd := m_nEffectStart + 3;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 500;
          end else if m_wAppearance in [68] then begin
            m_boUseEffect := True;
            m_nEffectStart := 60;
            m_nEffectFrame := m_nEffectStart;
            m_nEffectEnd := m_nEffectStart + 3;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 500;
          end else if m_wAppearance in [70..75, 90..91] then begin
            m_boUseEffect := True;
            m_nEffectStart := 4;
            m_nEffectFrame := m_nEffectStart;
            m_nEffectEnd := m_nEffectStart + 3;
            m_dwEffectStartTime := GetTickCount();
            m_dwEffectFrameTime := 500;
          end
        end
      end;
    SM_HIT: begin
        case m_wAppearance of
          54..58, 104..106, 110, 112..117, 121, 132, 133: begin //0710

            end;
          33, 34, 52: begin
              m_nStartFrame := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip);
              m_nEndFrame := m_nStartFrame + pm.ActStand.frame - 1;
              m_dwStartTime := GetTickCount;
              m_nDefFrameCount := pm.ActStand.frame;
            end;
          59, 70..75, 81..85, 90..92, 94..98, 111, 130, 131, 118..120, 122, 123: begin
              m_nStartFrame := pm.ActAttack.start;
              m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
              m_dwFrameTime := pm.ActAttack.ftime;
              m_dwStartTime := GetTickCount;
              if m_wAppearance = 84 then begin
                m_boDigUp := True;
                m_boUseEffect := True;
                m_nEffectStart := 14;
                m_nEffectFrame := m_nEffectStart;
                m_nEffectEnd := m_nEffectStart + 7;
                m_dwEffectStartTime := GetTickCount();
                m_dwEffectFrameTime := m_dwFrameTime;
              end;
            end;
        else begin
            m_nStartFrame := pm.ActAttack.start + m_btDir * (pm.ActAttack.frame + pm.ActAttack.skip);
            m_nEndFrame := m_nStartFrame + pm.ActAttack.frame - 1;
            m_dwFrameTime := pm.ActAttack.ftime;
            m_dwStartTime := GetTickCount;
            if m_wAppearance = 51 then begin
              m_boUseEffect := True;
              m_nEffectStart := 60;
              m_nEffectFrame := m_nEffectStart;
              m_nEffectEnd := m_nEffectStart + 7;
              m_dwEffectStartTime := GetTickCount();
              m_dwEffectFrameTime := 200;
            end;
          end;
        end;

      end;
    SM_DIGUP: begin
        if m_wAppearance = 52 then begin
          m_boDigUp := True;
          m_dwUseEffectTick := GetTickCount + 23000;
          Randomize;
          g_SndMgr.PlaySound(Random(7) + 146, m_nCurrX, m_nCurrY);
          m_boUseEffect := True;
          m_nEffectStart := 60;
          m_nEffectFrame := m_nEffectStart;
          m_nEffectEnd := m_nEffectStart + 11;
          m_dwEffectStartTime := GetTickCount();
          m_dwEffectFrameTime := 100;
        end;
        if m_wAppearance in [84, 85] then begin
          m_nStartFrame := pm.ActCritical.start;
          m_nEndFrame := m_nStartFrame + pm.ActCritical.frame - 1;
          m_dwFrameTime := pm.ActCritical.ftime;
          m_dwStartTime := GetTickCount;
        end;
        if m_wAppearance = 85 then begin
          m_boDigUp := True;
          m_boUseEffect := True;
          m_nEffectStart := 127;
          m_nEffectFrame := m_nEffectStart;
          m_nEffectEnd := m_nEffectStart + 34;
          m_dwEffectStartTime := GetTickCount();
          m_dwEffectFrameTime := m_dwFrameTime;
        end;
      end;
  end;
end;

constructor TNpcActor.Create;
begin
  inherited;
  m_EffSurface := nil;
  m_boHitEffect := False;
  m_nHitEffectNumber := 0;
  m_boDigUp := False;
end;

procedure TNpcActor.DrawChr(dsurface: TCustomCanvas; dx, dy: Integer; blend, boFlag: Boolean; DrawOnSale: Boolean);
var
  bCanDraw                  : Boolean;
  c                         : Integer;
  ceff                      : TColorEffect;
begin
  m_btDir := m_btDir mod 3;
  if GetTickCount - m_dwLoadSurfaceTime > (FREE_TEXTURE_TIME div 2) then begin
    m_dwLoadSurfaceTime := GetTickCount;
    LoadSurface;
  end;
  //if m_sUserName = '' then Exit;  //1015
  ceff := GetDrawEffectValue;
  if m_BodySurface <> nil then begin
    if m_wAppearance in [94..98] then begin
      dsurface.DrawBlend(
        dx + m_nPx + m_nShiftX,
        dy + m_nPy + m_nShiftY,
        m_BodySurface,
        1);
    end else if m_wAppearance = 51 then begin
      DrawEffSurface(dsurface,
        m_BodySurface,
        dx + m_nPx + m_nShiftX,
        dy + m_nPy + m_nShiftY,
        True,
        ceff);
    end else begin
      DrawEffSurface(dsurface,
        m_BodySurface,
        dx + m_nPx + m_nShiftX,
        dy + m_nPy + m_nShiftY,
        blend,
        ceff);
    end;
  end;

  if m_boUseEffect and (m_EffSurface <> nil) then begin //blue
    dsurface.DrawBlend(
      dx + m_nEffX + m_nShiftX,
      dy + m_nEffY + m_nShiftY,
      m_EffSurface,
      1);
  end;
end;

procedure TNpcActor.DrawEff(dsurface: TCustomCanvas; dx, dy: Integer);
begin
  {if m_boUseEffect and (m_EffSurface <> nil) then begin
    DrawBlend(dsurface,
      dx + m_nEffX + m_nShiftX,
      dy + m_nEffY + m_nShiftY,
      m_EffSurface,
      1);
  end;}
end;

function TNpcActor.GetDefaultFrame(wmode: Boolean): Integer;
var
  cf, dr                    : Integer;
  pm                        : pTMonsterAction;
begin
  Result := 0;
  pm := GetRaceByPM(m_btRace, m_wAppearance);
  if pm = nil then Exit;
  m_btDir := m_btDir mod 3;

  if m_nCurrentDefFrame < 0 then
    cf := 0
  else if m_nCurrentDefFrame >= pm.ActStand.frame then
    cf := 0
  else
    cf := m_nCurrentDefFrame;

  if m_wAppearance in [54..59, 94..98, 70..75, 81..85, 90..92, 112..123, 130..132] then
    Result := pm.ActStand.start + cf
  else
    Result := pm.ActStand.start + m_btDir * (pm.ActStand.frame + pm.ActStand.skip) + cf;
end;

procedure TNpcActor.LoadSurface;
var
  WMImage                   : TWMImages;
begin
  if (m_btRace = 50) {and not (m_wAppearance in [42..47, 54..58])} then begin
    if (m_wAppearance >= 100) and (m_wAppearance < 130) then begin
      if m_wAppearance in [112..123] then
        m_BodySurface := g_WNpc2ImgImages.GetCachedImage(m_nBodyOffset + 0, m_nPx, m_nPy)
      else
        m_BodySurface := g_WNpc2ImgImages.GetCachedImage(m_nBodyOffset + m_nCurrentFrame, m_nPx, m_nPy)
    end else begin
      if m_wAppearance in [130, 131] then
        m_BodySurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + 0, m_nPx, m_nPy)
      else
        m_BodySurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nCurrentFrame, m_nPx, m_nPy);
    end;
  end;

  if m_wAppearance in [42..47, 54..58] then
    m_BodySurface := nil;

  if m_BodySurface = nil then
    m_boVisible := False
  else
    m_boVisible := True;

  if m_boUseEffect then begin
    if m_wAppearance in [118..120, 122, 123] then begin
      m_EffSurface := g_WNpc2ImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
    end else if m_wAppearance = 131 then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
    end else if m_wAppearance in [54..58, 94..98] then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
    end else if m_wAppearance = 84 then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
    end else if m_wAppearance = 85 then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
    end else if m_wAppearance in [33..34] then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
    end else if m_wAppearance = 42 then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
      m_nEffX := m_nEffX + 71;
      m_nEffY := m_nEffY + 5;
    end else if m_wAppearance = 43 then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
      m_nEffX := m_nEffX + 71;
      m_nEffY := m_nEffY + 37;
    end else if m_wAppearance = 44 then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
      m_nEffX := m_nEffX + 7;
      m_nEffY := m_nEffY + 12;
    end else if m_wAppearance = 45 then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
      m_nEffX := m_nEffX + 6;
      m_nEffY := m_nEffY + 12;
    end else if m_wAppearance = 46 then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
      m_nEffX := m_nEffX + 7;
      m_nEffY := m_nEffY + 12;
    end else if m_wAppearance = 47 then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
      m_nEffX := m_nEffX + 8;
      m_nEffY := m_nEffY - 12;
    end else if m_wAppearance in [51, 52, 70..75, 90, 91] then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_nEffectFrame, m_nEffX, m_nEffY);
    end else if m_wAppearance in [60..67] then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(3540 + m_btDir * 10 + m_nEffectFrame, m_nEffX, m_nEffY);
    end else if m_wAppearance = 68 then begin
      m_EffSurface := g_WNpcImgImages.GetCachedImage(m_nBodyOffset + m_btDir * 10 + m_nEffectFrame, m_nEffX, m_nEffY);
    end;

  end;

end;

procedure TNpcActor.Run;
var
  nEffectFrame              : Integer;
  dwEffectFrameTime         : LongWord;
begin
  inherited Run;
  nEffectFrame := m_nEffectFrame;
  if m_boUseEffect then begin
    if m_boUseMagic then
      dwEffectFrameTime := Round(m_dwEffectFrameTime / 3)
    else
      dwEffectFrameTime := m_dwEffectFrameTime;

    if GetTickCount - m_dwEffectStartTime > dwEffectFrameTime then begin
      m_dwEffectStartTime := GetTickCount();
      if m_nEffectFrame < m_nEffectEnd then begin
        Inc(m_nEffectFrame);
      end else begin
        if m_boDigUp then begin
          if GetTickCount > m_dwUseEffectTick then begin
            m_boUseEffect := False;
            m_boDigUp := False;
            m_dwUseEffectTick := GetTickCount();
          end;
          m_nEffectFrame := m_nEffectStart;
        end else
          m_nEffectFrame := m_nEffectStart;
        m_dwEffectStartTime := GetTickCount();
      end;
    end;
  end;
  if nEffectFrame <> m_nEffectFrame then begin
    m_dwLoadSurfaceTime := GetTickCount();
    LoadSurface();
  end;
end;

end.

