{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library graphics classes          }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxAnimation;

{$I cxVer.inc}

interface

uses
  Windows, Types, SysUtils, Classes, Graphics, Math, dxCore, cxClasses,
  dxGDIPlusAPI, dxGdiPlusClasses, cxGeometry, cxGraphics, Forms, dxSmartImage;

type
  TdxAnimationTransition = class;
  TdxAnimationController = class;
  TdxDrawAnimationMode = (amScrollLeft, amScrollUp, amScrollRight, amScrollDown,
    amFade, amSegmentedFade, amRandomSegmentedFade,
    amScrollLeftFade, amScrollUpFade, amScrollRightFade, amScrollDownFade);

  TdxAnimationTransitionEffect = (ateLinear, ateAccelerateDecelerate, ateCubic,
    ateTanh, ateBack, ateBounce, ateCircle, ateElastic, ateExponential, ateSine, ateQuadratic, ateQuartic, ateCustom);
  TdxAnimationTransitionEffectMode = (atmIn, atmOut, atmInOut);

  TdxAnimationEvent = procedure(Sender: TdxAnimationTransition;
    var APosition: Integer; var AFinished: Boolean) of object;

  TdxAnimationTransitionEffectProc = function(Sender: TdxAnimationTransition;
    const AValue, AMaxValue: Int64; const ALength: Integer): Integer;

  IdxAnimationListener = interface
  ['{0CAAD87B-8A4B-464B-A738-1340BD80C3D8}']
    procedure AfterAnimation(Sender: TdxAnimationController);
    procedure BeforeAnimation(Sender: TdxAnimationController);
    procedure DestroyAnimation(Animation: TdxAnimationTransition);
  end;

  { TdxAnimationController }

  TdxAnimationController = class
  private
    FAnimations: TcxObjectList;
    FActiveAnimations: Integer;
    FListenerList: IInterfaceList;
    FTimer: TcxTimer;
    function GetAnimation(AIndex: Integer): TdxAnimationTransition;
    function GetCount: Integer;
  protected
    procedure CheckTimer;
    procedure Resume(Animation: TdxAnimationTransition);
    procedure Suspend(Animation: TdxAnimationTransition);
    procedure TimerHandler(Sender: TObject); virtual;
    procedure Terminate(Animation: TdxAnimationTransition);

    property Animations[Index: Integer]: TdxAnimationTransition read GetAnimation;
    property ActiveAnimations: Integer read FActiveAnimations write FActiveAnimations;
    property Count: Integer read GetCount;
    property ListenerList: IInterfaceList read FListenerList;
    property Timer: TcxTimer read FTimer write FTimer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(Animation: TdxAnimationTransition);
    procedure AddListener(AListener: IdxAnimationListener);
    procedure Remove(Animation: TdxAnimationTransition);
    procedure RemoveListener(AListener: IdxAnimationListener);
    procedure Update;
  end;

  { TdxAnimationTransition }

  TdxAnimationTransition = class
  strict private
    FCurrent: Int64;
    FFinish: Int64;
    FFinished: Boolean;
    FFreeOnTerminate: Boolean;
    FLength: Integer;
    FLockCount: Integer;
    FPosition: Integer;
    FPositionChanged: Boolean;
    FStart: Int64;
    FTime: Cardinal;
    FTimerInterval: Cardinal;
    FTransitionEffect: TdxAnimationTransitionEffect;
    FTransitionEffectProc: TdxAnimationTransitionEffectProc;
    FTransitionEffectMode: TdxAnimationTransitionEffectMode;

    FOnAfterAnimate: TdxAnimationEvent;
    FOnAnimate: TdxAnimationEvent;
    FOnBeforeAnimate: TdxAnimationEvent;
    FOnTerminate: TNotifyEvent;

    function GetFinished: Boolean;
    function GetSuspended: Boolean;
    procedure SetFinished(AValue: Boolean);
  protected
    FInfo: TObject;

    procedure Animate;
    procedure InitializeTime;
    procedure InitializeTransitionEffectProc; virtual;
    function IsCompatible(Animation: TdxAnimationTransition): Boolean; virtual;
    procedure DoAfterAnimate; virtual;
    procedure DoAnimate; virtual;
    procedure DoBeforeAnimate; virtual;
    procedure TryAnimate;

    property Current: Int64 read FCurrent write FCurrent;
    property Finish: Int64 read FFinish write FFinish;
    property Length: Integer read FLength;
    property Start: Int64 read FStart write FStart;
    property TimerInterval: Cardinal read FTimerInterval write FTimerInterval;
    property TransitionEffectMode: TdxAnimationTransitionEffectMode read FTransitionEffectMode;
  public
    constructor Create(ATime: Cardinal; ATransitionEffect: TdxAnimationTransitionEffect = ateLinear;
      ALength: Integer = -1; ATransitionEffectMode: TdxAnimationTransitionEffectMode = atmIn); virtual;
    destructor Destroy; override;
    procedure ImmediateAnimation;
    procedure RefreshAnimatedObject; virtual;
    procedure Resume;
    procedure Suspend(AFinished: Boolean = False);
    procedure Terminate;

    property TransitionEffect: TdxAnimationTransitionEffect read FTransitionEffect;
    property TransitionEffectProc: TdxAnimationTransitionEffectProc read FTransitionEffectProc;
    property Finished: Boolean read GetFinished write SetFinished;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
    property Info: TObject read FInfo write FInfo;
    property Position: Integer read FPosition;
    property PositionChanged: Boolean read FPositionChanged write FPositionChanged;
    property Suspended: Boolean read GetSuspended;
    property Time: Cardinal read FTime;

    property OnAfterAnimate: TdxAnimationEvent read FOnAfterAnimate write FOnAfterAnimate;
    property OnAnimate: TdxAnimationEvent read FOnAnimate write FOnAnimate;
    property OnBeforeAnimate: TdxAnimationEvent read FOnBeforeAnimate write FOnBeforeAnimate;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

  { TdxImageAnimationTransition }

  TdxImageAnimationTransition = class(TdxAnimationTransition)
  strict private
    FMode: TdxDrawAnimationMode;
  public
    constructor Create(const AStartImage, AFinishImage: TGraphic; ATime: Cardinal;
      AMode: TdxDrawAnimationMode; ATransition: TdxAnimationTransitionEffect = ateLinear;
      ALength: Integer = -1; AFreeImagesOnTerminate: Boolean = False); reintroduce; virtual;
    procedure Draw(ACanvas: TCanvas; const ADestRect: TRect); overload;
    procedure Draw(AGraphics: TdxGPGraphics; const ADestRect: TRect); overload;
    procedure DrawTransparent(ACanvas: TCanvas; const ADestRect: TRect);

    property Mode: TdxDrawAnimationMode read FMode;
  end;

  { TdxRectAnimationTransition }

  TdxRectAnimationTransition = class(TdxAnimationTransition)
  strict private
    FSourceRect: TRect;
    FTargetRect: TRect;

    function GetCurrentRect: TRect;
  public
    constructor Create(const ASourceRect, ATargetRect: TRect; ATime: Cardinal;
      ATransitionEffect: TdxAnimationTransitionEffect = ateLinear;
      ATransitionEffectMode: TdxAnimationTransitionEffectMode = atmIn); reintroduce;
    property CurrentRect: TRect read GetCurrentRect;
    property SourceRect: TRect read FSourceRect write FSourceRect;
    property TargetRect: TRect read FTargetRect write FTargetRect;
  end;

function dxAnimationController: TdxAnimationController;
function dxGetExactTickCount: Int64;
function dxGetExactTime(const AExactTickCount: Int64): Cardinal;
function dxMulDiv64(const nNumber, nNumerator, nDenominator: Int64): Int64;
function dxTimeToTickCount(const ATime: Cardinal): Int64;

implementation

type
  TdxEasingFunction = function(ANormalizedTime: Double): Double;

  { TdxAnimationInfoSegments }

  TdxAnimationInfoSegments = class
  private
    FDelay: array of array of Integer;
    FDest, FSource: array of array of TRect;
    FLeft, FTop, FRowCount, FColCount: Integer;
    FAnimationLengthPerSegment: Integer;
    function GetCount(var ASize, AItemSize: Integer): Integer;
  public
    constructor Create(const ALeft, ATop, AWidth, AHeight, ASegmentWidth, ASegmentHeight: Integer);
    destructor Destroy; override;
    procedure DrawImage(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer;
      AStartImage, AFinishImage: TdxGPImage; AProgress: Byte);
    procedure InitializeItems(AWidth, AHeight, ASegmentWidth, ASegmentHeight: Integer);
    procedure InitializeRandomAlpha;
    procedure PrepareDestItems(const ALeft, ATop: Integer);
  end;

  { TdxAnimationInfo }

  TdxAnimationInfo = class
  protected
    Destination: TcxBitmap32;
    FreeOriginalImages: Boolean;
    SegmentsInfo: TdxAnimationInfoSegments;
    StartImage, FinishImage: TdxGPImage;
    StartImageOriginal, FinishImageOriginal: TGraphic;
    StartImageWasCreated, FinishImageWasCreated: Boolean;

    procedure CalculateFadingScrollParameters(const AFullLength, AProgress: Integer; var ALength, AOffset: Integer; var AAlpha: Byte); inline;
    function CheckImage(ASourceImage: TGraphic; var ACheckedImage: TdxGPImage): Boolean;

    procedure DrawFade(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
    procedure DrawRandomSegmentedFade(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);

    procedure DrawScrollDown(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight, AOffset: Integer);
    procedure DrawScrollLeft(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight, AOffset: Integer);
    procedure DrawScrollRight(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight, AOffset: Integer);
    procedure DrawScrollUp(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight, AOffset: Integer);

    procedure DrawScrollDownFade(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
    procedure DrawScrollLeftFade(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
    procedure DrawScrollRightFade(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
    procedure DrawScrollUpFade(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);

    procedure DrawSegmentedFade(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
    function GetSegmentsInfo(const ALeft, ATop, AWidth, AHeight: Integer): TdxAnimationInfoSegments;
  public
    constructor Create(const AStartImage, AFinishImage: TGraphic; AFreeOriginalImages: Boolean = False);
    constructor CreateEx(const AStartImage, AFinishImage: TdxGPImage; AFreeOriginalImages: Boolean = False);
    destructor Destroy; override;
  end;

  { TdxEaseHelper }

  TdxEaseHelper = class
  private
    class function BackEasing(ANormalizedTime: Double): Double; static;
    class function BounceEasing(ANormalizedTime: Double): Double; static;
    class function CircleEasing(ANormalizedTime: Double): Double; static;
    class function CubicEasing(ANormalizedTime: Double): Double; static;
    class function DefaultEasing(ANormalizedTime: Double): Double; static;
    class function ElasticEasing(ANormalizedTime: Double): Double; static;
    class function ExponentialEasing(ANormalizedTime: Double): Double; static;
    class function SineEasing(ANormalizedTime: Double): Double; static;
    class function QuadraticEasing(ANormalizedTime: Double): Double; static;
    class function QuarticEasing(ANormalizedTime: Double): Double; static;

    class function GetFunction(AEffect: TdxAnimationTransitionEffect): TdxEasingFunction; static;
    class function GetNormalizedTime(const AValue, AMaxValue: Int64): Double; static;
    class function Ease(AMode: TdxAnimationTransitionEffectMode; AFunction: TdxEasingFunction;
      ANormalizedTime: Double): Double; static;
  protected
    class function Calculate(AMode: TdxAnimationTransitionEffectMode; AEffect: TdxAnimationTransitionEffect;
      const AValue, AMaxValue: Int64): Double; static;
  end;

var
  AnimationController: TdxAnimationController;

function dxAnimationController: TdxAnimationController;
begin
  if AnimationController = nil then
    AnimationController := TdxAnimationController.Create;
  Result := AnimationController;
end;

function dxMulDiv64(const nNumber, nNumerator, nDenominator: Int64): Int64;
var
  A: Integer;
begin
  A := nNumber;
  Result := Trunc(A * nNumerator / nDenominator);
end;

function dxGetExactTickCount: Int64;
begin
  if not QueryPerformanceCounter(Result) then
    Result := GetTickCount;
end;

function dxGetExactTime(const AExactTickCount: Int64): Cardinal; // in milliseconds
var
  AFreq: Int64;
begin
  if QueryPerformanceFrequency(AFreq) then
    Result := dxMulDiv64(1000, AExactTickCount, AFreq)
  else
    Result := AExactTickCount;
end;

function dxTimeToTickCount(const ATime: Cardinal): Int64;
var
  AFreq: Int64;
begin
  if QueryPerformanceFrequency(AFreq) then
    Result := dxMulDiv64(ATime, AFreq, 1000)
  else
    Result := ATime;
end;

function dxEaseTransitionEffect(AMode: TdxAnimationTransitionEffectMode; AEffect: TdxAnimationTransitionEffect;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := Round(ALength * TdxEaseHelper.Calculate(AMode, AEffect, AValue, AMaxValue));
end;

function dxLinearTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := dxMulDiv64(ALength, AValue, AMaxValue);
end;

function dxAccelerateDecelerateTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := Round(ALength * (-Power(AValue / AMaxValue - 1, 6) + 1));
end;

function dxCubicTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := dxEaseTransitionEffect(Sender.TransitionEffectMode, ateCubic, AValue, AMaxValue, ALength);
end;

function dxTanhTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
const
  AExactitude = 3;
var
  ATanh: Double;
begin
  ATanh := Tanh(AValue / AMaxValue * (2 * AExactitude) - AExactitude);
  Result := Trunc(ALength / (2 * Tanh(AExactitude)) * (ATanh - Tanh(-AExactitude)) + 0.5);
end;

function dxBackTransitionEffectProc(Sender: TdxAnimationTransition; const AValue, AMaxValue: Int64;
  const ALength: Integer): Integer;
begin
  Result := dxEaseTransitionEffect(Sender.TransitionEffectMode, ateBack, AValue, AMaxValue, ALength);
end;

function dxBounceTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := dxEaseTransitionEffect(Sender.TransitionEffectMode, ateBounce, AValue, AMaxValue, ALength);
end;

function dxCircleTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := dxEaseTransitionEffect(Sender.TransitionEffectMode, ateCircle, AValue, AMaxValue, ALength);
end;

function dxElasticTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := dxEaseTransitionEffect(Sender.TransitionEffectMode, ateElastic, AValue, AMaxValue, ALength);
end;

function dxExponentialTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := dxEaseTransitionEffect(Sender.TransitionEffectMode, ateExponential, AValue, AMaxValue, ALength);
end;

function dxSineTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := dxEaseTransitionEffect(Sender.TransitionEffectMode, ateSine, AValue, AMaxValue, ALength);
end;

function dxQuadraticTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := dxEaseTransitionEffect(Sender.TransitionEffectMode, ateQuadratic, AValue, AMaxValue, ALength);
end;

function dxQuarticTransitionEffectProc(Sender: TdxAnimationTransition;
  const AValue, AMaxValue: Int64; const ALength: Integer): Integer;
begin
  Result := dxEaseTransitionEffect(Sender.TransitionEffectMode, ateQuartic, AValue, AMaxValue, ALength);
end;

{ TdxAnimationController }

constructor TdxAnimationController.Create;
begin
  FListenerList := TInterfaceList.Create;
  FAnimations := TcxObjectList.Create;
  FTimer := TcxTimer.Create(nil);
  FTimer.Interval := 1;
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerHandler;
end;

destructor TdxAnimationController.Destroy;
begin
  FListenerList := nil;
  FTimer.Free;
  FAnimations.Free;
  AnimationController := nil;
  inherited Destroy;
end;

procedure TdxAnimationController.Add(Animation: TdxAnimationTransition);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if not Animations[I].IsCompatible(Animation) then
      Animations[I].Finished := True;
  FAnimations.Add(Animation);
end;

procedure TdxAnimationController.AddListener(AListener: IdxAnimationListener);
begin
  if FListenerList.IndexOf(AListener) = -1 then
    FListenerList.Add(AListener);
end;

procedure TdxAnimationController.Remove(Animation: TdxAnimationTransition);
var
  I: Integer;
begin
  for I := 0 to ListenerList.Count - 1 do
    (ListenerList[I] as IdxAnimationListener).DestroyAnimation(Animation);
  FAnimations.Remove(Animation);
end;

procedure TdxAnimationController.RemoveListener(
  AListener: IdxAnimationListener);
begin
  FListenerList.Remove(AListener);
end;

procedure TdxAnimationController.Update;
begin
  TimerHandler(nil);
end;

procedure TdxAnimationController.CheckTimer;
begin
  Timer.Enabled := ActiveAnimations > 0;
end;

procedure TdxAnimationController.Resume(Animation: TdxAnimationTransition);
begin
  Inc(FActiveAnimations);
  CheckTimer;
end;

procedure TdxAnimationController.Suspend(Animation: TdxAnimationTransition);
begin
  Dec(FActiveAnimations);
  CheckTimer;
end;

procedure TdxAnimationController.TimerHandler(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ListenerList.Count - 1 do
    (ListenerList[I] as IdxAnimationListener).BeforeAnimation(Self);
  try
    for I := Count - 1 downto 0 do
      Animations[I].TryAnimate;
  finally
    for I := 0 to ListenerList.Count - 1 do
      (ListenerList[I] as IdxAnimationListener).AfterAnimation(Self);
  end;
  for I := Count - 1 downto 0 do
    Animations[I].PositionChanged := False;
end;

procedure TdxAnimationController.Terminate(Animation: TdxAnimationTransition);
begin
  Dec(FActiveAnimations);
  CheckTimer;
end;

function TdxAnimationController.GetAnimation(AIndex: Integer): TdxAnimationTransition;
begin
  Result := TdxAnimationTransition(FAnimations.List[AIndex]);
end;

function TdxAnimationController.GetCount: Integer;
begin
  Result := FAnimations.Count;
end;

{ TdxAnimationInfoSegments }

constructor TdxAnimationInfoSegments.Create(
  const ALeft, ATop, AWidth, AHeight, ASegmentWidth, ASegmentHeight: Integer);
begin
  inherited Create;
  FTop := ATop;
  FLeft := ALeft;
  InitializeItems(AWidth, AHeight, ASegmentWidth, ASegmentHeight);
end;

function TdxAnimationInfoSegments.GetCount(var ASize, AItemSize: Integer): Integer;
begin
  while (AItemSize > 1) and (ASize div AItemSize < 5) do
    Dec(AItemSize);
  Result := ASize div AItemSize;
  if ASize mod AItemSize <> 0 then
    Inc(Result);
end;

destructor TdxAnimationInfoSegments.Destroy;
begin
  Finalize(FDest);
  Finalize(FSource);
  Finalize(FDelay);
  inherited Destroy;
end;

procedure TdxAnimationInfoSegments.DrawImage(AGraphics: TdxGPGraphics; const ALeft,
  ATop, AWidth, AHeight: Integer; AStartImage, AFinishImage: TdxGPImage; AProgress: Byte);
var
  ACellAlpha: Integer;
  ARow, ACol: Integer;
begin
  PrepareDestItems(ALeft, ATop);
  for ARow := 0 to FRowCount - 1 do
    for ACol := 0 to FColCount - 1 do
    begin
      ACellAlpha := MulDiv(MaxByte, AProgress - FDelay[ARow, ACol], FAnimationLengthPerSegment);
      ACellAlpha := Max(0, Min(MaxByte, ACellAlpha));
      if ACellAlpha < 255 then
        AGraphics.Draw(AStartImage, FDest[ARow, ACol], FSource[ARow, ACol]);
      if ACellAlpha > 0 then
        AGraphics.Draw(AFinishImage, FDest[ARow, ACol], FSource[ARow, ACol], ACellAlpha);
    end;
end;

procedure TdxAnimationInfoSegments.InitializeItems(
  AWidth, AHeight, ASegmentWidth, ASegmentHeight: Integer);
var
  ARow, ACol: Integer;
begin
  FColCount := GetCount(AWidth, ASegmentWidth);
  FRowCount := GetCount(AHeight, ASegmentHeight);
  FAnimationLengthPerSegment := Max(1, 100 div Min(FRowCount, FColCount));

  SetLength(FDest, FRowCount, FColCount);
  SetLength(FSource, FRowCount, FColCount);
  SetLength(FDelay, FRowCount, FColCount);

  for ARow := 0 to FRowCount - 1 do
    for ACol := 0 to FColCount - 1 do
    begin
      FSource[ARow, ACol] := Rect(ACol * ASegmentWidth, ARow * ASegmentHeight,
        Min((ACol + 1) * ASegmentWidth, AWidth), Min((ARow + 1) * ASegmentHeight, AHeight));
      FDest[ARow, ACol] := cxRectOffset(FSource[ARow, ACol], FLeft, FTop);
      FDelay[ARow, ACol] := MulDiv(100 - FAnimationLengthPerSegment, ACol + ARow + 2, FRowCount + FColCount);
    end;
end;

procedure TdxAnimationInfoSegments.InitializeRandomAlpha;
var
  ARow, ACol: Integer;
begin
  for ARow := 0 to FRowCount - 1 do
    for ACol := 0 to FColCount - 1 do
      FDelay[ARow, ACol] := Random(100 - FAnimationLengthPerSegment + 1);
end;

procedure TdxAnimationInfoSegments.PrepareDestItems(const ALeft, ATop: Integer);
var
  ARow, ACol: Integer;
begin
  if (FLeft <> ALeft) or (FTop <> ATop) then
  begin
    FLeft := ALeft;
    FTop := ATop;
    for ARow := 0 to FRowCount - 1 do
      for ACol := 0 to FColCount - 1 do
        FDest[ARow, ACol] := cxRectOffset(FSource[ARow, ACol], ALeft, ATop);
  end;
end;

{ TdxAnimationInfo }

constructor TdxAnimationInfo.Create(const AStartImage: TGraphic;
  const AFinishImage: TGraphic; AFreeOriginalImages: Boolean = False);
begin
  StartImageOriginal := AStartImage;
  FinishImageOriginal := AFinishImage;
  StartImageWasCreated := CheckImage(AStartImage, StartImage);
  FinishImageWasCreated := CheckImage(AFinishImage, FinishImage);
  Destination := TcxBitmap32.CreateSize(AStartImage.Width, AStartImage.Height);
  FreeOriginalImages := AFreeOriginalImages;
end;

constructor TdxAnimationInfo.CreateEx(const AStartImage: TdxGPImage;
  const AFinishImage: TdxGPImage; AFreeOriginalImages: Boolean = False);
begin
  FinishImage := AFinishImage;
  FinishImageOriginal := AFinishImage;
  StartImage := AStartImage;
  StartImageOriginal := AStartImage;
  FreeOriginalImages := AFreeOriginalImages;
end;

destructor TdxAnimationInfo.Destroy;
begin
  FreeAndNil(SegmentsInfo);
  FreeAndNil(Destination);
  if FreeOriginalImages then
  begin
    FreeAndNil(FinishImageOriginal);
    FreeAndNil(StartImageOriginal);
  end;
  if StartImageWasCreated then
    FreeAndNil(StartImage);
  if FinishImageWasCreated then
    FreeAndNil(FinishImage);
  inherited Destroy;
end;

procedure TdxAnimationInfo.CalculateFadingScrollParameters(const AFullLength, AProgress: Integer;
  var ALength, AOffset: Integer; var AAlpha: Byte);
const
  AScrollPercent = 20;
begin
  AAlpha := Min(255, Round(Sqr(AProgress / 6.25)));
  ALength := MulDiv(AFullLength, AScrollPercent, 100);
  AOffset := MulDiv(ALength, AProgress, 100);
end;

function TdxAnimationInfo.CheckImage(ASourceImage: TGraphic; var ACheckedImage: TdxGPImage): Boolean;
var
  B: TcxBitmap32;
begin
  Result := not ((ASourceImage is TdxGPImage) and (TdxGPImage(ASourceImage).ImageDataFormat = dxImageBitmap));

  if not Result then
    ACheckedImage := TdxGPImage(ASourceImage)
  else
  begin
    if ASourceImage is TBitmap then
      ACheckedImage := TdxGPImage.CreateFromBitmap(TBitmap(ASourceImage))
    else
    begin
      B := TcxBitmap32.CreateSize(ASourceImage.Width, ASourceImage.Height);
      try
        B.Canvas.Draw(0, 0, ASourceImage);
        ACheckedImage := TdxGPImage.CreateFromBitmap(B);
      finally
        B.Free;
      end;
    end;
  end;
  if Result and dxGPIsDoubleBufferedNeeded(cxScreenCanvas.Handle) then
    ACheckedImage.ImageDataFormat := dxImagePng;
end;

procedure TdxAnimationInfo.DrawFade(
  AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
begin
  AGraphics.Draw(StartImage, cxRectBounds(ALeft, ATop, AWidth, AHeight), Rect(0, 0, AWidth, AHeight));
  AGraphics.Draw(FinishImage, cxRectBounds(ALeft, ATop, AWidth, AHeight), Rect(0, 0, AWidth, AHeight), MulDiv(255, AProgress, 100));
end;

procedure TdxAnimationInfo.DrawRandomSegmentedFade(
  AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
var
  AWasInitialized: Boolean;
begin
  AWasInitialized := SegmentsInfo <> nil;
  with GetSegmentsInfo(ALeft, ATop, AWidth, AHeight) do
  begin
    if not AWasInitialized then
      InitializeRandomAlpha;
    DrawImage(AGraphics, ALeft, ATop, AWidth, AHeight, StartImage, FinishImage, AProgress);
  end;
end;

procedure TdxAnimationInfo.DrawScrollDown(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight, AOffset: Integer);
begin
  AGraphics.Draw(StartImage, cxRectBounds(ALeft, ATop + AOffset, AWidth, AHeight));
  AGraphics.Draw(FinishImage, cxRectBounds(ALeft, ATop - AHeight + AOffset, AWidth, AHeight));
end;

procedure TdxAnimationInfo.DrawScrollLeft(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight, AOffset: Integer);
begin
  AGraphics.Draw(StartImage, cxRectBounds(ALeft - AOffset, ATop, AWidth, AHeight));
  AGraphics.Draw(FinishImage, cxRectBounds(ALeft + AWidth - AOffset, ATop, AWidth, AHeight));
end;

procedure TdxAnimationInfo.DrawScrollRight(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight, AOffset: Integer);
begin
  AGraphics.Draw(StartImage, cxRectBounds(ALeft + AOffset, ATop, AWidth, AHeight));
  AGraphics.Draw(FinishImage, cxRectBounds(ALeft - AWidth + AOffset, ATop, AWidth, AHeight));
end;

procedure TdxAnimationInfo.DrawScrollUp(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight, AOffset: Integer);
begin
  AGraphics.Draw(StartImage, cxRectBounds(ALeft, ATop - AOffset, AWidth, AHeight));
  AGraphics.Draw(FinishImage, cxRectBounds(ALeft, ATop + AHeight - AOffset, AWidth, AHeight));
end;

procedure TdxAnimationInfo.DrawScrollDownFade(
  AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
var
  AAlpha: Byte;
  ALength, AOffset: Integer;
begin
  CalculateFadingScrollParameters(AHeight, AProgress, ALength, AOffset, AAlpha);
  AGraphics.Draw(StartImage, cxRectBounds(ALeft, ATop + AOffset, AWidth, AHeight), Rect(0, 0, AWidth, AHeight), 255 - AAlpha);
  AGraphics.Draw(FinishImage, cxRectBounds(ALeft, ATop - ALength + AOffset, AWidth, AHeight), Rect(0, 0, AWidth, AHeight), AAlpha);
end;

procedure TdxAnimationInfo.DrawScrollLeftFade(
  AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
var
  AAlpha: Byte;
  ALength, AOffset: Integer;
begin
  CalculateFadingScrollParameters(AWidth, AProgress, ALength, AOffset, AAlpha);
  AGraphics.Draw(StartImage, cxRectBounds(ALeft - AOffset, ATop, AWidth, AHeight), Rect(0, 0, AWidth, AHeight), 255 - AAlpha);
  AGraphics.Draw(FinishImage, cxRectBounds(ALeft + ALength - AOffset, ATop, AWidth, AHeight), Rect(0, 0, AWidth, AHeight), AAlpha);
end;

procedure TdxAnimationInfo.DrawScrollRightFade(
  AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
var
  AAlpha: Byte;
  ALength, AOffset: Integer;
begin
  CalculateFadingScrollParameters(AWidth, AProgress, ALength, AOffset, AAlpha);
  AGraphics.Draw(StartImage, cxRectBounds(ALeft + AOffset, ATop, AWidth, AHeight), Rect(0, 0, AWidth, AHeight), 255 - AAlpha);
  AGraphics.Draw(FinishImage, cxRectBounds(ALeft - ALength + AOffset, ATop, AWidth, AHeight), Rect(0, 0, AWidth, AHeight), AAlpha);
end;

procedure TdxAnimationInfo.DrawScrollUpFade(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
var
  AAlpha: Byte;
  ALength, AOffset: Integer;
begin
  CalculateFadingScrollParameters(AHeight, AProgress, ALength, AOffset, AAlpha);
  AGraphics.Draw(StartImage, cxRectBounds(ALeft, ATop - AOffset, AWidth, AHeight), Rect(0, 0, AWidth, AHeight), 255 - AAlpha);
  AGraphics.Draw(FinishImage, cxRectBounds(ALeft, ATop + ALength - AOffset, AWidth, AHeight), Rect(0, 0, AWidth, AHeight), AAlpha);
end;

procedure TdxAnimationInfo.DrawSegmentedFade(AGraphics: TdxGPGraphics; const ALeft, ATop, AWidth, AHeight: Integer; AProgress: Byte);
begin
  GetSegmentsInfo(ALeft, ATop, AWidth, AHeight).DrawImage(AGraphics, ALeft, ATop, AWidth, AHeight, StartImage, FinishImage, AProgress);
end;

function TdxAnimationInfo.GetSegmentsInfo(const ALeft, ATop, AWidth, AHeight: Integer): TdxAnimationInfoSegments;
const
  MaxSegmentSize = 100;
begin
  if SegmentsInfo = nil then
    SegmentsInfo := TdxAnimationInfoSegments.Create(ALeft, ATop, AWidth, AHeight,
      Min(MaxSegmentSize, Max(1, AWidth div 10)), Min(MaxSegmentSize, Max(1, AHeight div 10)));
  Result := SegmentsInfo;
end;

{ TdxEaseHelper }

class function TdxEaseHelper.Calculate(AMode: TdxAnimationTransitionEffectMode; AEffect: TdxAnimationTransitionEffect;
  const AValue, AMaxValue: Int64): Double;
begin
  Result := Ease(AMode, GetFunction(AEffect), GetNormalizedTime(AValue, AMaxValue));
end;

class function TdxEaseHelper.BackEasing(ANormalizedTime: Double): Double;
begin
  Result := Power(ANormalizedTime, 3) - ANormalizedTime * 0.3 * Sin(Pi * ANormalizedTime);
end;

class function TdxEaseHelper.BounceEasing(ANormalizedTime: Double): Double;

  function GetDegreeBounces(ABounciness, ATime, ANormalizedTime: Double): Double;
  begin
    Result := Ln(-ANormalizedTime * ATime * (1.0 - ABounciness) + 1.0) / Ln(ABounciness);
    if Result < 0 then
      Result := -Ceil(Abs(Result))
    else
      Result := Floor(Result);
  end;

var
  ABounces, ABounciness, ATime, ADegreeBounce: Double;
  ACorrectionFactorNumerator, ACorrectionFactorDenominator, ADenominatorResult: Double;
begin
  ABounces := 3;
  ABounciness := 5;
  ATime := (1.0 - Power(ABounciness, ABounces)) / (1.0 - ABounciness) + Power(ABounciness, ABounces) * 0.5;
  ADegreeBounce := GetDegreeBounces(ABounciness, ATime, ANormalizedTime);
  ACorrectionFactorNumerator := (1.0 - Power(ABounciness, ADegreeBounce)) / ((1.0 - ABounciness) * ATime);
  ACorrectionFactorDenominator := (1.0 - Power(ABounciness, ADegreeBounce + 1)) / ((1.0 - ABounciness) * ATime);
  ATime := ANormalizedTime - (ACorrectionFactorNumerator + ACorrectionFactorDenominator) * 0.5;
  ADenominatorResult := ANormalizedTime - ATime - ACorrectionFactorNumerator;
  Result := -Power(1.0 / ABounciness, ABounces - ADegreeBounce) / (ADenominatorResult * ADenominatorResult) *
    (ATime - ADenominatorResult) * (ATime + ADenominatorResult);
end;

class function TdxEaseHelper.CircleEasing(ANormalizedTime: Double): Double;
var
  ATime: Double;
begin
  ATime := Max(0.0, Min(1.0, ANormalizedTime));
  Result := 1 - Sqrt(1 - ATime * ATime);
end;

class function TdxEaseHelper.CubicEasing(ANormalizedTime: Double): Double;
begin
  Result := Power(ANormalizedTime, 3);
end;

class function TdxEaseHelper.DefaultEasing(ANormalizedTime: Double): Double;
begin
  Result := -Power(ANormalizedTime - 1, 6) + 1;
end;

class function TdxEaseHelper.ElasticEasing(ANormalizedTime: Double): Double;
var
  AShift, ASpringiness, AOscillations: Double;
begin
  AOscillations := 2;
  ASpringiness := 7.5;
  AShift := (Exp(ASpringiness * ANormalizedTime) - 1) / (Exp(ASpringiness) - 1);
  Result := AShift * Sin((Pi * 2 * AOscillations + Pi / 2) * ANormalizedTime);
end;

class function TdxEaseHelper.ExponentialEasing(ANormalizedTime: Double): Double;
var
  AExponent: Double;
begin
  AExponent := 7;
  Result := (Exp(AExponent * ANormalizedTime) - 1.0) / (Exp(AExponent) - 1);
end;

class function TdxEaseHelper.SineEasing(ANormalizedTime: Double): Double;
begin
  Result := 1 - Sin(Pi / 2 * (1 - ANormalizedTime));
end;

class function TdxEaseHelper.QuadraticEasing(ANormalizedTime: Double): Double;
begin
  Result := ANormalizedTime * ANormalizedTime;
end;

class function TdxEaseHelper.QuarticEasing(ANormalizedTime: Double): Double;
begin
  Result := Power(ANormalizedTime, 4);
end;

class function TdxEaseHelper.GetFunction(AEffect: TdxAnimationTransitionEffect): TdxEasingFunction;
begin
  case AEffect of
    ateBack:
      Result := @TdxEaseHelper.BackEasing;
    ateBounce:
      Result := @TdxEaseHelper.BounceEasing;
    ateCircle:
      Result := @TdxEaseHelper.CircleEasing;
    ateCubic:
      Result := @TdxEaseHelper.CubicEasing;
    ateElastic:
      Result := @TdxEaseHelper.ElasticEasing;
    ateExponential:
      Result := @TdxEaseHelper.ExponentialEasing;
    ateSine:
      Result := @TdxEaseHelper.SineEasing;
    ateQuadratic:
      Result := @TdxEaseHelper.QuadraticEasing;
    ateQuartic:
      Result := @TdxEaseHelper.QuarticEasing;
    else
      Result := @TdxEaseHelper.DefaultEasing;
  end;
end;

class function TdxEaseHelper.GetNormalizedTime(const AValue, AMaxValue: Int64): Double;
begin
  Result := AValue / AMaxValue;
end;

class function TdxEaseHelper.Ease(AMode: TdxAnimationTransitionEffectMode; AFunction: TdxEasingFunction;
  ANormalizedTime: Double): Double;
begin
  case AMode of
    atmIn:
      Result := AFunction(ANormalizedTime);
    atmOut:
      Result := 1 - AFunction(1 - ANormalizedTime);
    else
      if ANormalizedTime >= 0.5 then
        Result := (1.0 - AFunction((1.0 - AnormalizedTime) * 2.0)) * 0.5 + 0.5
      else
        Result := AFunction(AnormalizedTime * 2.0) * 0.5;
  end;
end;

{ TdxAnimationTransition }

constructor TdxAnimationTransition.Create(ATime: Cardinal;
  ATransitionEffect: TdxAnimationTransitionEffect = ateLinear; ALength: Integer = -1;
  ATransitionEffectMode: TdxAnimationTransitionEffectMode = atmIn);
begin
  inherited Create;
  FLength := ALength;
  FTime := ATime;
  FTransitionEffect := ATransitionEffect;
  FTransitionEffectMode := ATransitionEffectMode;
  InitializeTransitionEffectProc;
  FLockCount := 1;
  FFreeOnTerminate := True;
  dxAnimationController.Add(Self);
end;

destructor TdxAnimationTransition.Destroy;
begin
  if AnimationController <> nil then
    AnimationController.Remove(Self);
  FreeAndNil(FInfo);
  inherited Destroy;
end;

procedure TdxAnimationTransition.Animate;
var
  APosition: Integer;
begin
  FCurrent := Min(FFinish, dxGetExactTickCount);
  FFinished := FFinished or (FCurrent >= FFinish);
  if FFinished then
  begin
    FCurrent := FFinish;
    APosition := FLength;
  end
  else
    APosition := TransitionEffectProc(Self, FCurrent - FStart, FFinish - FStart, FLength);
  if (APosition <> FPosition) or (FPosition = 0) or Finished then
  begin
    PositionChanged := True;
    FPosition := APosition;
    DoBeforeAnimate;
    DoAnimate;
    DoAfterAnimate;
  end;
end;

procedure TdxAnimationTransition.DoAfterAnimate;
var
  AFinished: Boolean;
begin
  AFinished := Finished;
  if Assigned(FOnAfterAnimate) then
    FOnAfterAnimate(Self, FPosition, AFinished);
  FFinished := AFinished;
  if Finished then
    CallNotify(FOnTerminate, Self);
end;

procedure TdxAnimationTransition.DoAnimate;
var
  AFinished: Boolean;
begin
  AFinished := Finished;
  if Assigned(FOnAnimate) then
    FOnAnimate(Self, FPosition, AFinished);
  Finished := AFinished;
end;

procedure TdxAnimationTransition.DoBeforeAnimate;
var
  AFinished: Boolean;
begin
  AFinished := Finished;
  if Assigned(FOnBeforeAnimate) then
    FOnBeforeAnimate(Self, FPosition, AFinished);
  Finished := AFinished;
end;

procedure TdxAnimationTransition.ImmediateAnimation;
begin
  InitializeTime;
  while not Finished do
    Animate;
  if Finished and FreeOnTerminate then
    Free;
end;

procedure TdxAnimationTransition.InitializeTime;
begin
  if FCurrent <> 0 then
  begin
    FCurrent := FCurrent - FStart;
    FStart := dxGetExactTickCount - FCurrent;
    FCurrent := FStart + FCurrent;
  end
  else
    FStart := dxGetExactTickCount;
  FFinish := FStart + dxTimeToTickCount(FTime);
end;

procedure TdxAnimationTransition.InitializeTransitionEffectProc;
begin
  case FTransitionEffect of
    ateAccelerateDecelerate:
      FTransitionEffectProc := dxAccelerateDecelerateTransitionEffectProc;
    ateCubic:
      FTransitionEffectProc := dxCubicTransitionEffectProc;
    ateTanh:
      FTransitionEffectProc := dxTanhTransitionEffectProc;
    ateBack:
      FTransitionEffectProc := dxBackTransitionEffectProc;
    ateBounce:
      FTransitionEffectProc := dxBounceTransitionEffectProc;
    ateCircle:
      FTransitionEffectProc := dxCircleTransitionEffectProc;
    ateElastic:
      FTransitionEffectProc := dxElasticTransitionEffectProc;
    ateExponential:
      FTransitionEffectProc := dxExponentialTransitionEffectProc;
    ateSine:
      FTransitionEffectProc := dxSineTransitionEffectProc;
    ateQuadratic:
      FTransitionEffectProc := dxQuadraticTransitionEffectProc;
    ateQuartic:
      FTransitionEffectProc := dxQuarticTransitionEffectProc;
  else
    FTransitionEffectProc := dxLinearTransitionEffectProc;
  end;
end;

function TdxAnimationTransition.IsCompatible(Animation: TdxAnimationTransition): Boolean;
begin
  Result := True;
end;

procedure TdxAnimationTransition.RefreshAnimatedObject;
begin
end;

procedure TdxAnimationTransition.Resume;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
  begin
    InitializeTime;
    dxAnimationController.Resume(Self);
  end;
end;

procedure TdxAnimationTransition.Suspend(AFinished: Boolean = False);
begin
  Inc(FLockCount);
  if (FLockCount = 1) and AFinished then
  begin
    dxAnimationController.Suspend(Self);
    Terminate;
  end;
end;

procedure TdxAnimationTransition.Terminate;
begin
  Finished := True;
  if FreeOnTerminate then
    Free;
end;

procedure TdxAnimationTransition.TryAnimate;
begin
  if Suspended then Exit;
  if not Finished and (dxGetExactTickCount < FFinish) then
    Animate
  else
    Finished := True;
  if Finished and FreeOnTerminate then
    Free;
end;

function TdxAnimationTransition.GetFinished: Boolean;
begin
  Result := FFinished;
end;

function TdxAnimationTransition.GetSuspended: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TdxAnimationTransition.SetFinished(AValue: Boolean);
begin
  if AValue <> FFinished then
  begin
    FCurrent := FFinish;
    FFinished := AValue;
    if not FFinished then
    begin
      FCurrent := 0;
      InitializeTime;
    end;
    Animate;
  end;
end;

{ TdxImageAnimationTransition }

constructor TdxImageAnimationTransition.Create(const AStartImage, AFinishImage: TGraphic; ATime: Cardinal;
  AMode: TdxDrawAnimationMode; ATransition: TdxAnimationTransitionEffect = ateLinear;
  ALength: Integer = -1; AFreeImagesOnTerminate: Boolean = False);
begin
  FMode := AMode;
  if FMode in [amFade .. amScrollDownFade] then
    ALength := 100
  else
    if ALength = -1 then
    begin
      if FMode in [amScrollLeft, amScrollRight] then
        ALength := AStartImage.Width
      else
        ALength := AStartImage.Height;
    end;
  FInfo := TdxAnimationInfo.Create(AStartImage, AFinishImage, AFreeImagesOnTerminate);
  inherited Create(ATime, ATransition, ALength);
end;

procedure TdxImageAnimationTransition.Draw(ACanvas: TCanvas; const ADestRect: TRect);
var
  ATempCanvas: TCanvas;
  ARect: TRect;
begin
  ATempCanvas := TdxAnimationInfo(FInfo).Destination.Canvas;
  ARect := cxRectSetNullOrigin(ADestRect);

  dxGPPaintCanvas.BeginPaint(ATempCanvas.Handle, ARect);
  try
    Draw(dxGPPaintCanvas, ARect);
  finally
    dxGPPaintCanvas.EndPaint;
  end;

  BitBlt(ACanvas.Handle, ADestRect.Left, ADestRect.Top, cxRectWidth(ADestRect), cxRectHeight(ADestRect), ATempCanvas.Handle, 0, 0, SRCCOPY);
end;

procedure TdxImageAnimationTransition.Draw(AGraphics: TdxGPGraphics; const ADestRect: TRect);
var
  AInfo: TdxAnimationInfo;
  AWidth, AHeight: Integer;
begin
  AInfo := TdxAnimationInfo(Info);
  AWidth := ADestRect.Right - ADestRect.Left;
  AHeight := ADestRect.Bottom - ADestRect.Top;
  case Mode of
    amScrollRight:
      AInfo.DrawScrollRight(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
    amScrollUp:
      AInfo.DrawScrollUp(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
    amScrollLeft:
      AInfo.DrawScrollLeft(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
    amScrollDown:
      AInfo.DrawScrollDown(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
    amFade:
      AInfo.DrawFade(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
    amSegmentedFade:
      AInfo.DrawSegmentedFade(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
    amRandomSegmentedFade:
      AInfo.DrawRandomSegmentedFade(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
    amScrollLeftFade:
      AInfo.DrawScrollLeftFade(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
    amScrollUpFade:
      AInfo.DrawScrollUpFade(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
    amScrollRightFade:
      AInfo.DrawScrollRightFade(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
    amScrollDownFade:
      AInfo.DrawScrollDownFade(AGraphics, ADestRect.Left, ADestRect.Top, AWidth, AHeight, Position);
  end;
end;

procedure TdxImageAnimationTransition.DrawTransparent(ACanvas: TCanvas; const ADestRect: TRect);
var
  ATempBuffer: TcxBitmap32;
begin
  ATempBuffer := TdxAnimationInfo(FInfo).Destination;
  ATempBuffer.Clear;

  dxGPPaintCanvas.BeginPaint(ATempBuffer.Canvas.Handle, ADestRect);
  try
    Draw(dxGPPaintCanvas, cxRect(cxRectSize(ADestRect)));
    cxAlphaBlend(ACanvas.Handle, ATempBuffer, ADestRect, ATempBuffer.ClientRect);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

{ TdxRectAnimationTransition }

constructor TdxRectAnimationTransition.Create(
  const ASourceRect, ATargetRect: TRect; ATime: Cardinal;
  ATransitionEffect: TdxAnimationTransitionEffect;
  ATransitionEffectMode: TdxAnimationTransitionEffectMode);
var
  ALength: Integer;
begin
  FSourceRect := ASourceRect;
  FTargetRect := ATargetRect;

  ALength := Abs(ASourceRect.Left - ATargetRect.Left);
  ALength := Max(ALength, Abs(ASourceRect.Top - ATargetRect.Top));
  ALength := Max(ALength, Abs(ASourceRect.Left - ATargetRect.Left));
  ALength := Max(ALength, Abs(ASourceRect.Right - ATargetRect.Right));

  inherited Create(ATime, ATransitionEffect, ALength, ATransitionEffectMode);
end;

function TdxRectAnimationTransition.GetCurrentRect: TRect;

  function Calculate(ASourceValue, ATargetValue: Integer): Integer;
  begin
    if ASourceValue <> ATargetValue then
      Result := MulDiv(ASourceValue, Length - Position, Length) + MulDiv(ATargetValue, Position, Length)
    else
      Result := ASourceValue;
  end;

begin
  Result.Top := Calculate(SourceRect.Top, TargetRect.Top);
  Result.Left := Calculate(SourceRect.Left, TargetRect.Left);
  Result.Right := Calculate(SourceRect.Right, TargetRect.Right);
  Result.Bottom := Calculate(SourceRect.Bottom, TargetRect.Bottom);
end;

initialization

finalization
  FreeAndNil(AnimationController);

end.
