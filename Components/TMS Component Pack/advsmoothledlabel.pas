{**************************************************************************}
{ TAdvSmoothLedLabel component                                             }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2009 - 2012                                                }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvSmoothLedLabel;

interface

{$I TMSDEFS.INC}

uses
  SysUtils, Classes, Controls, Graphics, Messages, Windows, Math,
  GDIPFill, AdvStyleIF,
  AdvGDIP
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 3; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.1.0.0 : New : Support DateTime
  //          : New : Property Format to format the led value
  //          : Fixed : Access violation when choosing gtNone
  //          : Improved : Property Angle to use with gradienttype gtAngle
  // v1.1.0.1 : Fixed : Issue with Separator TimeFormat
  // v1.1.0.2 : Fixed : Issue with Negative height or width drawing
  // v1.1.0.3 : Fixed : number seven display corrected


type
  TAdvSmoothLedLabelBackGroundPosition = (bpTopLeft,bpTopCenter,bpTopRight,bpBottomLeft,bpBottomCenter,bpBottomRight,bpTiled,bpStretched,bpCenterLeft,bpCenterCenter,bpCenterRight);

  TAdvSmoothLedLabelCaption = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothLedLabel = class(TGraphicControl, ITMSStyle)
  private
    FCount: integer;
    FPainting: Boolean;
    FDesignTime: Boolean;
    FCaption: TAdvSmoothLedLabelCaption;
    FTransparent: Boolean;
    FFill: TGDIPFill;
    FOnChange: TNotifyEvent;
    procedure SetCaption(const Value: TAdvSmoothLedLabelCaption);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetTransparent(const Value: Boolean);
    procedure SetFill(const Value: TGDIPFill);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure CaptionChanged(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    function GetVersionNr: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function Getcount: integer;
    function IsPainting: Boolean;
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
    procedure DrawLedLabel(ACanvas: TCanvas);
    procedure SaveToImage(Filename: String; ImageWidth, ImageHeight: integer; ImageType: TImageType = itBMP; ImageQualityPercentage: integer = 100);
  published
    property Fill: TGDIPFill read FFill write SetFill;
    property Caption: TAdvSmoothLedLabelCaption read FCaption write SetCaption;
    property Version: string read GetVersion write SetVersion;
    property Transparent: Boolean read FTransparent write SetTransparent default false;

    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Visible;
    property ShowHint;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnResize;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnStartDock;
    property OnStartDrag;
    property PopupMenu;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothLedLabelValueType = (vtNormal, vtDateTime);

  TAdvSmoothLedLabelCaption = class(TPersistent)
  private
    FOwner: TAdvSmoothLedLabel;
    FGradientType: TAdvGradientType;
    FPicture: TAdvGDIPPicture;
    FEndColor: TColor;
    FHatchStyle: THatchStyle;
    FStartOpacity: Byte;
    FValue: Double;
    FTop: integer;
    FLeft: integer;
    FEndOpacity: Byte;
    FStartColor: TColor;
    FOnChange: TNotifyEvent;
    FColorOff: TColor;
    FColorOffOpacity: Byte;
    FAngle: integer;
    FFormat: String;
    FTimeFormat: String;
    FTimeValue: TDateTime;
    FValueType: TAdvSmoothLedLabelValueType;
    procedure SetEndColor(const Value: TColor);
    procedure SetEndOpacity(const Value: Byte);
    procedure SetGradientType(const Value: TAdvGradientType);
    procedure SetHatchStyle(const Value: THatchStyle);
    procedure SetLeft(const Value: integer);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetStartColor(const Value: TColor);
    procedure SetStartOpacity(const Value: Byte);
    procedure SetTop(const Value: integer);
    procedure SetValue(const Value: Double);
    procedure SetColorOff(const Value: TColor);
    procedure SetColorOffOpacity(const Value: Byte);
    procedure SetAngle(const Value: integer);
    procedure SetFormat(const Value: String);
    procedure SetTimeFormat(const Value: String);
    procedure SetTimeValue(const Value: TDateTime);
    procedure SetValueType(const Value: TAdvSmoothLedLabelValueType);
  protected
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothLedLabel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Value: Double read FValue write SetValue;
    property Left: integer read FLeft write SetLeft default 0;
    property Top: integer read FTop write SetTop default 0;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property ColorStart: TColor read FStartColor write SetStartColor default $004080FF;
    property ColorEnd: TColor read FEndColor write SetEndColor default clRed;
    property ColorOff: TColor read FColorOff write SetColorOff default clGray;
    property ColorOffOpacity: Byte read FColorOffOpacity write SetColorOffOpacity default 255;
    property OpacityStart: Byte read FStartOpacity write SetStartOpacity default 255;
    property OpacityEnd: Byte read FEndOpacity write SetEndOpacity default 255;
    property GradientType: TAdvGradientType read FGradientType write SetGradientType default gtVertical;
    property HatchStyle: THatchStyle read FHatchStyle write SetHatchStyle default HatchStyleHorizontal;
    property Angle: integer read FAngle write SetAngle default 0;
    property Format: String read FFormat write SetFormat;
    property TimeFormat: String read FTimeFormat write SetTimeFormat;
    property ValueType: TAdvSmoothLedLabelValueType read FValueType write SetValueType default vtnormal;
    property TimeValue: TDateTime read FTimeValue write SetTimeValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


implementation

procedure TAdvSmoothLedLabel.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothLedLabel) then
  begin
    FFill.Assign((Source as TAdvSmoothLedLabel).Fill);
    FCaption.Assign((Source as TAdvSmoothLedLabel).Caption);
    FTransparent := (Source as TAdvSmoothLedLabel).Transparent;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabel.CaptionChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothLedLabel.Changed;
begin
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothLedLabel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];

  FTransparent := false;
  Width := 200;
  Height := 50;
  FCaption := TAdvSmoothLedLabelCaption.Create(Self);
  FCaption.OnChange := CaptionChanged;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  if FDesignTime then
  begin
    FFill.Color := clBlack;
    FFill.ColorTo := clGray;    
  end;
end;

destructor TAdvSmoothLedLabel.Destroy;
begin
  Fill.Free;
  Caption.Free;
  inherited;
end;

procedure TAdvSmoothLedLabel.DrawLedLabel(ACanvas: TCanvas);
var
  r: TGPRectF;
  g: TGPGraphics;
begin
  if (Height <= 0) or (Width <= 0) then
    Exit;

  FPainting := true;

  g := TGPGraphics.Create(ACanvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  r := MakeRect(0, 0, Width - 1, Height - 1);

  if not Transparent then
  begin
    ////Label Rectangle///
    r := Fill.Fill(g, r);
    //////////////////////
  end;
  DrawLed(g, MakeRect(0, 0, Width, Height), TFillValueType(Caption.ValueType), Caption.Format, Caption.TimeFormat, Caption.Value,
    Caption.TimeValue, Caption.ColorOff, Caption.ColorOffOpacity, Caption.ColorStart, Caption.ColorEnd, Caption.OpacityStart,
      Caption.OpacityEnd, Caption.GradientType, Caption.Angle, Caption.HatchStyle, Caption.Picture);
  g.Free;
  FPainting := false;
  Inc(FCount);
end;

procedure TAdvSmoothLedLabel.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothLedLabel.Getcount: integer;
begin
  Result := FCount;
end;

function TAdvSmoothLedLabel.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothLedLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothLedLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvSmoothLedLabel.IsPainting: Boolean;
begin
  Result := FPainting;
end;

procedure TAdvSmoothLedLabel.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothLedLabel.Paint;
begin
  DrawLedLabel(Canvas);
end;

procedure TAdvSmoothLedLabel.SaveToImage(Filename: String; ImageWidth,
  ImageHeight: integer; ImageType: TImageType; ImageQualityPercentage: integer);
var
  img, finalimg: graphics.TBitmap;
  gpimg: TGPImage;
  g: TGPGraphics;
  enc: TEncoderParameters;
begin
  img := nil;
  gpimg := nil;
  g := nil;
  finalimg := nil;
  try
    img := graphics.TBitmap.Create;
    img.Width := Width;
    img.Height := Height;

    DrawLedLabel(img.Canvas);

    finalimg := graphics.TBitmap.Create;
    finalimg.Width := ImageWidth;
    finalimg.Height := ImageHeight;
    finalimg.Canvas.StretchDraw(Bounds(0, 0, ImageWidth, ImageHeight), img);

    gpimg := TGPImage.Create(CreateStream(finalimg));

    enc := GetEncoderQualityParameters(ImageQualityPercentage);

    gpimg.Save(filename, GetCLSID(ImageType), @enc);

  finally
    gpimg.Free;
    finalimg.Free;
    g.Free;
    img.Free;
  end;
end;

procedure TAdvSmoothLedLabel.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothLedLabel.SetCaption(const Value: TAdvSmoothLedLabelCaption);
begin
  if FCaption <> value then
  begin
    FCaption.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothLedLabel.SetComponentStyle(AStyle: TTMSStyle);
begin
  // TODO : do color settings here
end;

procedure TAdvSmoothLedLabel.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothLedLabel.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabel.SetVersion(const Value: string);
begin

end;

procedure TAdvSmoothLedLabel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.result := 1;
end;

{ TAdvSmoothLedLabelCaption }

procedure TAdvSmoothLedLabelCaption.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothLedLabelCaption then
  begin
    FLeft := (Source as TAdvSmoothLedLabelCaption).Left;
    FTop := (Source as TAdvSmoothLedLabelCaption).Top;
    FStartColor := (Source as TAdvSmoothLedLabelCaption).ColorStart;
    FEndColor := (Source as TAdvSmoothLedLabelCaption).ColorEnd;
    FGradientType := (Source as TAdvSmoothLedLabelCaption).GradientType;
    FHatchStyle := (Source as TAdvSmoothLedLabelCaption).HatchStyle;
    FStartOpacity := (Source as TAdvSmoothLedLabelCaption).OpacityStart;
    FEndOpacity := (Source as TAdvSmoothLedLabelCaption).OpacityEnd;
    FPicture.Assign((Source as TAdvSmoothLedLabelCaption).Picture);
    FValue := (Source as TAdvSmoothLedLabelCaption).Value;
    FColorOff := (Source as TAdvSmoothLedLabelCaption).ColorOff;
    FColorOffOpacity := (Source as TAdvSmoothLedLabelCaption).ColorOffOpacity;
    FAngle := (source as TAdvSmoothLedLabelCaption).Angle;
    FFormat := (Source as TAdvSmoothLedLabelCaption).Format;
    FTimeFormat := (Source as TAdvSmoothLedLabelCaption).TimeFormat;
    FTimeValue := (Source as TAdvSmoothLedLabelCaption).TimeValue;
    FValueType := (Source as TAdvSmoothLedLabelCaption).ValueType;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  FOwner.CaptionChanged(Self);
end;

constructor TAdvSmoothLedLabelCaption.Create(AOwner: TAdvSmoothLedLabel);
begin
  FOwner := AOwner;
  FStartColor := $004080FF;
  FEndColor := clRed;
  FStartOpacity := 255;
  FEndOpacity := 255;
  FGradientType := gtVertical;
  FHatchStyle := HatchStyleHorizontal;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FValue := 0;
  FColorOff := clGray;
  FColorOffOpacity := 255;
  FAngle := 0;
  FFormat := '000.00';
  FTimeFormat := 'hh:nn:ss';
  FValueType := vtNormal;
  FTimeValue := Now;
end;

destructor TAdvSmoothLedLabelCaption.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TAdvSmoothLedLabelCaption.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothLedLabelCaption.SetAngle(const Value: integer);
begin
  if FAngle <> value then
  begin
    FAngle := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetColorOff(const Value: TColor);
begin
  if FColorOff <> value then
  begin
    FColorOff := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetColorOffOpacity(const Value: Byte);
begin
  if FColorOffOpacity <> value then
  begin
    FColorOffOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetTimeFormat(const Value: String);
begin
  if FTimeFormat <> value then
  begin
    FTimeFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetEndColor(const Value: TColor);
begin
  if FEndColor <> value then
  begin
    FEndColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetEndOpacity(const Value: Byte);
begin
  if FEndOpacity <> value  then
  begin
    FEndOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetFormat(const Value: String);
begin
  if FFormat <> value then
  begin
    FFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetGradientType(
  const Value: TAdvGradientType);
begin
  if FGradientType <> value then
  begin
    FGradientType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetHatchStyle(const Value: THatchStyle);
begin
  if FHatchStyle <> value then
  begin
    FHatchStyle := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetLeft(const Value: integer);
begin
  if FLeft <> value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetPicture(
  const Value: TAdvGDIPPicture);
begin
  if FPicture <> value then
  begin
    FPicture.Assign(Value);
    PictureChanged(Self);
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetStartColor(const Value: TColor);
begin
  if FStartColor <> Value then
  begin
    FStartColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetStartOpacity(const Value: Byte);
begin
  if FStartOpacity <> value then
  begin
    FStartOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetValue(const Value: Double);
begin
  if FValue <> value then
  begin
    FValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetTimeValue(const Value: TDateTime);
begin
  if FTimeValue <> value then
  begin
    FTimeValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetValueType(
  const Value: TAdvSmoothLedLabelValueType);
begin
  if FvalueType <> value then
  begin
    FValueType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothLedLabelCaption.SetTop(const Value: integer);
begin
  if FTop <> value then
  begin
    FTop := Value;
    Changed;
  end;
end;

end.
