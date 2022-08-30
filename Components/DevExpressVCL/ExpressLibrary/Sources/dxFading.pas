{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library controls                  }
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

unit dxFading;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, SysUtils, Graphics, ExtCtrls,
  dxCore, dxCoreGraphics, dxCoreClasses, cxGraphics, dxGDIPlusClasses, dxGDIPlusApi, cxClasses,
  cxLookAndFeelPainters;

const
  dxFadeInDefaultAnimationFrameCount: Integer = 4;
  dxFadeInDefaultAnimationFrameDelay: Integer = 15;
  dxFadeOutDefaultAnimationFrameCount: Integer = 12;
  dxFadeOutDefaultAnimationFrameDelay: Integer = 20;

  dxFadeMaxAlpha = 255;
  dxFadeMaxAnimationCount = 20;
  dxFadeMaxAnimationFrameCount = 32;
  dxFadeMaxAnimationFrameDelay = 300;
  dxFadeTimerInterval = 10;

type
  TdxFader = class;
  TdxFadingList = class;

  TdxFadingObjectState = (fosNone, fosGetParams, fosFading);

  {TdxFadingOptions}

  TdxFadingOptions = class(TPersistent)
  private
    FFadeInFrameCount: Integer;
    FFadeInFrameDelay: Integer;
    FFadeOutFrameCount: Integer;
    FFadeOutFrameDelay: Integer;

    function IsFadeInFrameCountStored: Boolean;
    function IsFadeInFrameDelayStored: Boolean;
    function IsFadeOutFrameCountStored: Boolean;
    function IsFadeOutFrameDelayStored: Boolean;

    procedure SetFadeInFrameCount(AValue: Integer);
    procedure SetFadeInFrameDelay(AValue: Integer);
    procedure SetFadeOutFrameCount(AValue: Integer);
    procedure SetFadeOutFrameDelay(AValue: Integer);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property FadeInFrameCount: Integer read FFadeInFrameCount write SetFadeInFrameCount stored IsFadeInFrameCountStored;
    property FadeInFrameDelay: Integer read FFadeInFrameDelay write SetFadeInFrameDelay stored IsFadeInFrameDelayStored;
    property FadeOutFrameCount: Integer read FFadeOutFrameCount write SetFadeOutFrameCount stored IsFadeOutFrameCountStored;
    property FadeOutFrameDelay: Integer read FFadeOutFrameDelay write SetFadeOutFrameDelay stored IsFadeOutFrameDelayStored;
  end;

  IdxFadingObject = interface
  ['{73AB2A92-CDD9-4F13-965A-DC799DE837F9}']
    function CanFade: Boolean;
    procedure DrawFadeImage;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
  end;

  IdxFadingObjectFadingOptions = interface
  ['{FAAAF4F8-7F10-42CD-9368-2BB5A1F1482B}']
    function GetFadingOptions: TdxFadingOptions;
  end;

  { TdxFadingElement }

  TdxFadingState = (fsFadeIn, fsFadeOut);

  TdxFadingElement = class(TcxIUnknownObject)
  private
    FAnimationFrameCount: Integer;
    FAnimationFrameDelay: Integer;
    FDelayCount: Integer;
    FDelayIndex: Integer;
    FElement: TObject;
    FFadingObject: IdxFadingObject;
    FImageSize: TSize;
    FOwner: TdxFader;
    FStage: Integer;
    FWorkImage: TdxGPImage;
    procedure SetWorkImage(AImage: TdxGPImage);
  protected
    FImage1Colors: TRGBColors;
    FImage2Colors: TRGBColors;
    procedure BuildWorkImage;
    procedure ProcessFadeStep;
    procedure SetAnimationParams(AAnimationFrameCount, AAnimationFrameDelay: Integer);
    procedure SetImages(AImage1, AImage2: TBitmap);
    //
    property ImageSize: TSize read FImageSize;
    property Owner: TdxFader read FOwner;
  public
    constructor Create(AOwner: TdxFader; AElement: TObject);
    destructor Destroy; override;
    function DrawImage(DC: HDC; const R: TRect): Boolean;
    procedure Finalize;
    //
    property AnimationFrameCount: Integer read FAnimationFrameCount;
    property AnimationFrameDelay: Integer read FAnimationFrameDelay;
    property Element: TObject read FElement;
    property WorkImage: TdxGPImage read FWorkImage write SetWorkImage;
    property Stage: Integer read FStage write FStage;
  end;

  { TdxFadingObjectHelper }

  TdxFadingObjectHelper = class(TcxIUnknownObject, IdxFadingObject)
  private
    function GetActive: Boolean;
  protected
    // IdxFadingObject
    function CanFade: Boolean; virtual;
    procedure DrawFadeImage; virtual;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); virtual;
  public
    destructor Destroy; override;
    procedure CheckStartFading(APrevState, AState: TcxButtonState);
    function DrawImage(DC: HDC; const R: TRect): Boolean;
    procedure FadeIn;
    procedure FadeOut;
    procedure StopFading;
    //
    property Active: Boolean read GetActive;
  end;

  { TdxFadingList }

  TdxFadingList = class(TList)
  private
    function GetItems(Index: Integer): TdxFadingElement;
  public
    procedure Clear; override;
    property Items[Index: Integer]: TdxFadingElement read GetItems; default;
  end;

  { TdxFadingHelper }

  TdxFadingHelper = class
  public
    class function CheckColor(const AColor: TColor): TColor;
    class procedure CorrectAlphaChannel(ABitmap: TBitmap); overload;
    class procedure CorrectAlphaChannel(var AColors: TRGBColors); overload;
  end;

  { TdxFader }

  TdxFaderAnimationState = (fasDefault, fasEnabled, fasDisabled);

  TdxFader = class(TObject)
  private
    FList: TdxFadingList;
    FMaxAnimationCount: Integer;
    FState: TdxFaderAnimationState;
    FTimer: TTimer;
    function GetActive: Boolean;
    function GetIsReady: Boolean;
    function GetSystemAnimationState: LongBool;
    procedure SetMaxAnimationCount(Value: Integer);
    procedure ValidateQueue;
  protected
    function CanFade(AObject: TObject): Boolean; overload;
    function CanFade(AObject: TObject; out AFadingObject: IdxFadingObject): Boolean; overload;
    procedure DoTimer(Sender: TObject);
    //
    procedure AddFadingElement(AElement: TdxFadingElement);
    procedure RemoveFadingElement(AElement: TdxFadingElement);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function DrawFadeImage(AObject: TObject; DC: HDC; const R: TRect): Boolean;
    function Find(AObject: TObject; out AFadingElement: TdxFadingElement): Boolean;
    procedure Remove(AObject: TObject; ADestroying: Boolean = True);
    //
    procedure Fade(AObject: TObject; AState: TdxFadingState); overload;
    procedure Fade(AObject: TObject; AImage1, AImage2: TBitmap;
      AAnimationFrameCount, AAnimationFrameDelay: Integer); overload;
    procedure FadeIn(AObject: TObject);
    procedure FadeOut(AObject: TObject);
    //
    property Active: Boolean read GetActive;
    property IsReady: Boolean read GetIsReady;
    property MaxAnimationCount: Integer read FMaxAnimationCount write SetMaxAnimationCount;
    property State: TdxFaderAnimationState read FState write FState;
  end;

function dxFader: TdxFader;

implementation

uses
  Math, cxGeometry, cxControls;

var
  Fader: TdxFader;

function dxFader: TdxFader;
begin
  Result := Fader;
end;

{ TdxFadingElement }

constructor TdxFadingElement.Create(AOwner: TdxFader; AElement: TObject);
begin
  inherited Create;
  FElement := AElement;
  FOwner := AOwner;
  Supports(AElement, IdxFadingObject, FFadingObject);
end;

destructor TdxFadingElement.Destroy;
begin
  Owner.RemoveFadingElement(Self);
  FFadingObject := nil;
  FreeAndNil(FWorkImage);
  FImage1Colors := nil;
  FImage2Colors := nil;
  inherited Destroy;
end;

procedure TdxFadingElement.BuildWorkImage;
var
  AAlpha1, AAlpha2: Single;
  AColor1, AColor2, ATempColor: PRGBQuad;
  ATempImageColors: TRGBColors;
  I: Integer;
begin
  SetLength(ATempImageColors, Length(FImage1Colors));
  try
    AAlpha2 := MulDiv(Stage, dxFadeMaxAlpha, AnimationFrameCount) / dxFadeMaxAlpha;
    AAlpha1 := 1 - AAlpha2;

    AColor1 := @FImage1Colors[0];
    AColor2 := @FImage2Colors[0];
    ATempColor := @ATempImageColors[0];
    for I := 0 to Length(ATempImageColors) - 1 do
    begin
      ATempColor^.rgbBlue := Round(Min(MaxByte, AColor1^.rgbBlue * AAlpha1 + AColor2^.rgbBlue * AAlpha2));
      ATempColor^.rgbGreen := Round(Min(MaxByte, AColor1^.rgbGreen * AAlpha1 + AColor2^.rgbGreen * AAlpha2));
      ATempColor^.rgbRed := Round(Min(MaxByte, AColor1^.rgbRed * AAlpha1 + AColor2^.rgbRed * AAlpha2));
      ATempColor^.rgbReserved := Round(Min(MaxByte, AColor1^.rgbReserved * AAlpha1 + AColor2^.rgbReserved * AAlpha2));
      Inc(ATempColor);
      Inc(AColor1);
      Inc(AColor2);
    end;
    WorkImage := TdxGPImage.CreateFromBits(ImageSize.cx, ImageSize.cy, ATempImageColors, True);
  finally
    ATempImageColors := nil;
  end;
end;

function TdxFadingElement.DrawImage(DC: HDC; const R: TRect): Boolean;
begin
  Result := Assigned(WorkImage);
  if Result then
    WorkImage.StretchDraw(DC, R);
end;

procedure TdxFadingElement.Finalize;
begin
  WorkImage := nil;
  Free;
end;

procedure TdxFadingElement.ProcessFadeStep;
begin
  FDelayIndex := (FDelayIndex + 1) mod FDelayCount;
  if FDelayIndex = 0 then
  begin
    if Stage >= AnimationFrameCount then
      Finalize
    else
    begin
      Inc(FStage);
      BuildWorkImage;
    end;
  end;
end;

procedure TdxFadingElement.SetAnimationParams(AAnimationFrameCount, AAnimationFrameDelay: Integer);
begin
  FAnimationFrameCount := Max(1, Min(AAnimationFrameCount, dxFadeMaxAnimationFrameCount));
  FAnimationFrameDelay := Max(1, Min(AAnimationFrameDelay, dxFadeMaxAnimationFrameDelay));
  FDelayCount := Max(AnimationFrameDelay div dxFadeTimerInterval, 1);
  FDelayIndex := 0;
end;

procedure TdxFadingElement.SetImages(AImage1, AImage2: TBitmap);

  procedure InitializeImageColors(var AColors: TRGBColors; AImage: TBitmap);
  begin
    GetBitmapBits(AImage, AColors, True);
    TdxFadingHelper.CorrectAlphaChannel(AColors);
  end;

begin
  InitializeImageColors(FImage1Colors, AImage1);
  InitializeImageColors(FImage2Colors, AImage2);
  FImageSize := cxSize(AImage1.Width, AImage1.Height);
end;

procedure TdxFadingElement.SetWorkImage(AImage: TdxGPImage);
var
  ATemp: TdxGPImage;
begin
  if AImage <> FWorkImage then
  begin
    ATemp := FWorkImage;
    try
      FWorkImage := AImage;
      if FFadingObject <> nil then
        FFadingObject.DrawFadeImage;
    finally
      ATemp.Free;
    end;
  end;
end;

{ TdxFadingList }

procedure TdxFadingList.Clear;
begin
  while Count > 0 do
    Items[0].Free;
  inherited Clear;
end;

function TdxFadingList.GetItems(Index: Integer): TdxFadingElement;
begin
  Result := TdxFadingElement(inherited Items[Index]);
end;

{ TdxFadingHelper }

class function TdxFadingHelper.CheckColor(const AColor: TColor): TColor;
begin
  Result := ColorToRGB(AColor);
  if Result = 0 then
    Result := RGB(1, 1, 1);
end;

class procedure TdxFadingHelper.CorrectAlphaChannel(ABitmap: TBitmap);
var
  AColors: TRGBColors;
begin
  GetBitmapBits(ABitmap, AColors, True);
  CorrectAlphaChannel(AColors);
  SetBitmapBits(ABitmap, AColors, True);
end;

class procedure TdxFadingHelper.CorrectAlphaChannel(var AColors: TRGBColors);
var
  I: Integer;
  Q: PRGBQuad;
begin
  if Length(AColors) > 0 then
  begin
    Q := @AColors[0];
    for I := 0 to Length(AColors) - 1 do
    begin
      if (Q^.rgbReserved = 0) and (Q^.rgbBlue + Q^.rgbGreen + Q^.rgbRed > 0) then
        Q^.rgbReserved := MaxByte;
      Inc(Q);
    end;
  end;
end;

{ TdxFader }

constructor TdxFader.Create;
begin
  inherited Create;
  FState := fasDefault;
  FList := TdxFadingList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := dxFadeTimerInterval;
  FTimer.OnTimer := DoTimer;
  FMaxAnimationCount := 10;
end;

destructor TdxFader.Destroy;
begin
  Clear;
  FreeAndNil(FTimer);
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxFader.Clear;
begin
  FList.Clear;
end;

function TdxFader.CanFade(AObject: TObject): Boolean;
var
  AFadingObject: IdxFadingObject;
begin
  Result := CanFade(AObject, AFadingObject);
end;

function TdxFader.CanFade(AObject: TObject; out AFadingObject: IdxFadingObject): Boolean;
begin
  Result := IsReady and Supports(AObject, IdxFadingObject, AFadingObject) and AFadingObject.CanFade;
end;

function TdxFader.Contains(AObject: TObject): Boolean;
var
  AFadingElement: TdxFadingElement;
begin
  Result := Find(AObject, AFadingElement);
end;

function TdxFader.DrawFadeImage(AObject: TObject; DC: HDC; const R: TRect): Boolean;
var
  AFadingElement: TdxFadingElement;
begin
  Result := Find(AObject, AFadingElement) and AFadingElement.DrawImage(DC, R);
end;

procedure TdxFader.FadeIn(AObject: TObject);
begin
  Fade(AObject, fsFadeIn);
end;

procedure TdxFader.FadeOut(AObject: TObject);
begin
  Fade(AObject, fsFadeOut);
end;

procedure TdxFader.Fade(AObject: TObject; AState: TdxFadingState);
var
  AFadingOptions: TdxFadingOptions;
  AFadingObject: IdxFadingObject;
  AFadingObjectFadingOptions: IdxFadingObjectFadingOptions;
  ATemp1, ATemp2: TcxBitmap;
begin
  if CanFade(AObject, AFadingObject) then
  begin
    AFadingObject.GetFadingImages(ATemp1, ATemp2);
    AFadingOptions := TdxFadingOptions.Create;
    try
      if Supports(AFadingObject, IdxFadingObjectFadingOptions, AFadingObjectFadingOptions) and
        (AFadingObjectFadingOptions.GetFadingOptions <> nil)
      then
        AFadingOptions.Assign(AFadingObjectFadingOptions.GetFadingOptions);

      if AState = fsFadeIn then
        Fade(AObject, ATemp1, ATemp2, AFadingOptions.FadeInFrameCount, AFadingOptions.FadeInFrameDelay)
      else
        Fade(AObject, ATemp2, ATemp1, AFadingOptions.FadeOutFrameCount, AFadingOptions.FadeOutFrameDelay);
    finally
      FreeAndNil(AFadingOptions);
    end;
  end;
end;

procedure TdxFader.Fade(AObject: TObject; AImage1: TBitmap;
  AImage2: TBitmap; AAnimationFrameCount, AAnimationFrameDelay: Integer);

  function CanStartFading: Boolean;
  begin
    Result := CanFade(AObject) and (AImage1 <> nil) and (AImage2 <> nil) and
      (AImage1.Width = AImage2.Width) and (AImage1.Height = AImage2.Height) and not
      (AImage1.Empty or AImage2.Empty) and (AAnimationFrameCount > 0);
  end;

var
  AElement: TdxFadingElement;
begin
  try
    if CanStartFading then
    begin
      if not Find(AObject, AElement) then
      begin
        AElement := TdxFadingElement.Create(Self, AObject);
        AElement.SetAnimationParams(AAnimationFrameCount, AAnimationFrameDelay);
        AddFadingElement(AElement);
      end
      else
        if (AElement.ImageSize.cx = AImage1.Width) and (AElement.ImageSize.cy = AImage1.Height) then
        begin
          AImage1.Free;
          AImage1 := AElement.WorkImage.GetAsBitmap;
          AElement.SetAnimationParams(AAnimationFrameCount, AAnimationFrameDelay);
          AElement.Stage := 0;
        end;

      AElement.SetImages(AImage1, AImage2);
      AElement.BuildWorkImage;
    end;
  finally
    AImage1.Free;
    AImage2.Free;
  end;
end;

procedure TdxFader.DoTimer(Sender: TObject);
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    FList.Items[I].ProcessFadeStep;
end;

function TdxFader.GetSystemAnimationState: LongBool;
begin
  dxSystemInfo.GetParameter(SPI_GETMENUANIMATION, Result);
end;

procedure TdxFader.AddFadingElement(AElement: TdxFadingElement);
begin
  FList.Add(AElement);
  ValidateQueue;
end;

procedure TdxFader.RemoveFadingElement(AElement: TdxFadingElement);
begin
  FList.Remove(AElement);
  ValidateQueue;
end;

function TdxFader.Find(AObject: TObject; out AFadingElement: TdxFadingElement): Boolean;
var
  I: Integer;
begin
  AFadingElement := nil;
  for I := 0 to FList.Count - 1 do
    if FList[I].Element = AObject then
    begin
      AFadingElement := FList[I];
      Break;
    end;
  Result := AFadingElement <> nil;
end;

procedure TdxFader.Remove(AObject: TObject; ADestroying: Boolean = True);
var
  AElement: TdxFadingElement;
begin
  if Find(AObject, AElement) then
  begin
    if ADestroying then
      AElement.Free
    else
      AElement.Finalize;
  end;
end;

function TdxFader.GetActive: Boolean;
begin
  if State = fasDefault then
    Result := GetSystemAnimationState
  else
    Result := State = fasEnabled;
end;

function TdxFader.GetIsReady: Boolean;
begin
  Result := Active and (MaxAnimationCount > 0) and
    CheckGdiPlus and (GetDeviceCaps(cxScreenCanvas.Handle, BITSPIXEL) > 16);
end;

procedure TdxFader.SetMaxAnimationCount(Value: Integer);
begin
  Value := Min(Max(0, Value), dxFadeMaxAnimationCount);
  if FMaxAnimationCount <> Value then
  begin
    FMaxAnimationCount := Value;
    ValidateQueue;
  end;
end;

procedure TdxFader.ValidateQueue;
begin
  while FList.Count > MaxAnimationCount do
    FList[0].Finalize;
  FTimer.Enabled := FList.Count > 0;
end;

{ TdxFadingObjectHelper }

destructor TdxFadingObjectHelper.Destroy;
begin
  dxFader.Remove(Self);
  inherited Destroy;
end;

function TdxFadingObjectHelper.CanFade: Boolean;
begin
  Result := False;
end;

procedure TdxFadingObjectHelper.CheckStartFading(APrevState, AState: TcxButtonState);
begin
  if APrevState <> AState then
  begin
    if (AState = cxbsHot) and (APrevState in [cxbsNormal, cxbsDefault]) then
      FadeIn
    else
      if (AState in [cxbsNormal, cxbsDefault]) and (APrevState = cxbsHot) then
        FadeOut
      else
        StopFading;
  end;
end;

procedure TdxFadingObjectHelper.DrawFadeImage;
begin
end;

function TdxFadingObjectHelper.DrawImage(DC: HDC; const R: TRect): Boolean;
begin
  Result := dxFader.DrawFadeImage(Self, DC, R);
end;

procedure TdxFadingObjectHelper.FadeIn;
begin
  dxFader.FadeIn(Self);
end;

procedure TdxFadingObjectHelper.FadeOut;
begin
  dxFader.FadeOut(Self);
end;

procedure TdxFadingObjectHelper.GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap);
begin
end;

function TdxFadingObjectHelper.GetActive: Boolean;
begin
  Result := dxFader.Contains(Self);
end;

procedure TdxFadingObjectHelper.StopFading;
begin
  dxFader.Remove(Self, False);
end;

{ TdxFadingOptions }

constructor TdxFadingOptions.Create;
begin
  FFadeInFrameCount := dxFadeInDefaultAnimationFrameCount;
  FFadeInFrameDelay := dxFadeInDefaultAnimationFrameDelay;
  FFadeOutFrameCount := dxFadeOutDefaultAnimationFrameCount;
  FFadeOutFrameDelay := dxFadeOutDefaultAnimationFrameDelay;
end;

procedure TdxFadingOptions.Assign(Source: TPersistent);
begin
  if Source is TdxFadingOptions then
  begin
    FadeInFrameCount := TdxFadingOptions(Source).FadeInFrameCount;
    FadeInFrameDelay := TdxFadingOptions(Source).FadeInFrameDelay;
    FadeOutFrameCount := TdxFadingOptions(Source).FFadeOutFrameCount;
    FadeOutFrameDelay := TdxFadingOptions(Source).FFadeOutFrameDelay;
  end
  else
    inherited Assign(Source);
end;

function TdxFadingOptions.IsFadeInFrameCountStored: Boolean;
begin
  Result := FadeInFrameCount <> dxFadeInDefaultAnimationFrameCount;
end;

function TdxFadingOptions.IsFadeInFrameDelayStored: Boolean;
begin
  Result := FadeInFrameDelay <> dxFadeInDefaultAnimationFrameDelay;
end;

function TdxFadingOptions.IsFadeOutFrameCountStored: Boolean;
begin
  Result := FadeOutFrameCount <> dxFadeOutDefaultAnimationFrameCount;
end;

function TdxFadingOptions.IsFadeOutFrameDelayStored: Boolean;
begin
  Result := FadeOutFrameDelay <> dxFadeOutDefaultAnimationFrameDelay;
end;

procedure TdxFadingOptions.SetFadeInFrameCount(AValue: Integer);
begin
  FFadeInFrameCount := Max(0, Min(AValue, dxFadeMaxAnimationFrameCount));
end;

procedure TdxFadingOptions.SetFadeInFrameDelay(AValue: Integer);
begin
  FFadeInFrameDelay := Max(dxFadeTimerInterval, Min(AValue, dxFadeMaxAnimationFrameDelay));
end;

procedure TdxFadingOptions.SetFadeOutFrameCount(AValue: Integer);
begin
  FFadeOutFrameCount := Max(0, Min(AValue, dxFadeMaxAnimationFrameCount));
end;

procedure TdxFadingOptions.SetFadeOutFrameDelay(AValue: Integer);
begin
  FFadeOutFrameDelay := Max(dxFadeTimerInterval, Min(AValue, dxFadeMaxAnimationFrameDelay));
end;

initialization
  Fader := TdxFader.Create;

finalization
  FreeAndNil(Fader);

end.

