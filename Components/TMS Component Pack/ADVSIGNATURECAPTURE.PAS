{***********************************************************************}
{ TAdvSignatureCapture component                                        }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2014                                           }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit AdvSignatureCapture;

{$I TMSDEFS.INC}

{$R AdvSignatureCapture.res}

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, Graphics, Types,
  Generics.Collections, pngimage, jpeg, Dialogs, Forms
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.0.0  : First release
  // 1.0.1.0  : New: Empty property added

type
  TStoredPoint = class(TObject)
  private
  protected
  public
    X, Y, PenWidth: Integer;
    DrawLine: Boolean;
    PenColor: TColor;
  end;

  TTextHintPosition = (pBottomCenter, pBottomLeft, pBottomRight, pTopCenter, pTopLeft, pTopRight, pXY, pNone);

  TClearPosition = class(TPersistent)
  private
    FPicture: TPicture;
    FX: integer;
    FY: integer;
    FPosition: TTextHintPosition;
    FOnChange: TNotifyEvent;
    procedure SetPicture(const Value: TPicture);
    procedure SetPosition(const Value: TTextHintPosition);
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property X: integer read FX write SetX default 5;
    property Y: integer read FY write SetY default 25;
    property Position: TTextHintPosition read FPosition write SetPosition default pBottomLeft;
    property Picture: TPicture read FPicture write SetPicture;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TTextPosition = class(TPersistent)
  private
    FX: integer;
    FY: integer;
    FOnChange: TNotifyEvent;
    FLineWidth: integer;
    FLineColor: TColor;
    FPosition: TTextHintPosition;
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
    procedure SetLineColor(const Value: TColor);
    procedure SetLineWidth(const Value: integer);
    procedure SetPosition(const Value: TTextHintPosition);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property X: integer read FX write SetX default 25;
    property Y: integer read FY write SetY default 25;
    property Position: TTextHintPosition read FPosition write SetPosition default pBottomCenter;
    property LineColor: TColor read FLineColor write SetLineColor default clGray;
    property LineWidth: integer read FLineWidth write SetLineWidth default 2;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TPaintEvent = procedure(const Sender: TObject; const ACanvas: TCanvas; const ARect: TRect) of object;

  TAdvSignatureCapture = class(TGraphicControl)
  private
    { Private declarations }
    FPen: TPen;
    FText: string;
    FDrawing: Boolean;
    FSignaturePoints: TList<TStoredPoint>;
    FBorderColor: TColor;
    FBorderStyle: TBorderStyle;
    FOnPaint: TPaintEvent;
    FTextPos: TTextPosition;
    FClearSig: TClearPosition;
    FClearDown: boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure WriteStringToStream(const Stream: TStream; const s: string);
    function ReadStringFromStream(const Stream: TStream): string;
    procedure DrawSignature;
    procedure SetText(const Value: string);
    procedure SetPen(const Value: TPen);
    function GetColorEx: TColor;
    procedure SetColorEx(const Value: TColor);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetBorderColor(const Value: TColor);
    procedure SetTextPos(const Value: TTextPosition);
    procedure SetClearSig(const Value: TClearPosition);
    function GetClearRect: TRect;
    function GetEmpty: boolean;
    procedure SetEmpty(const Value: boolean);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure PenChanged(Sender: TObject);
    procedure TextPosChanged(Sender: TObject);
    procedure ClearPosChanged(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure Paint; override;
    procedure DoPaint(const ACanvas:TCanvas; const ARect: TRect); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(FileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToImageFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure SaveToStream(AStream: TStream);
    property Empty: boolean read GetEmpty write SetEmpty;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ClearSig: TClearPosition read FClearSig write SetClearSig;
    property Color:TColor read GetColorEx write SetColorEx default clWhite;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Pen: TPen read FPen write SetPen;
    property PopupMenu;
    property ShowHint;
    property Tag;
    property Text: string read FText write SetText;
    property TextPos: TTextPosition read FTextPos write SetTextPos;
    property Top;
    property Touch;
    property Visible;
    property Version: string read GetVersion write SetVersion;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

const
  CLEARMARGIN = 4;
  TEXTMARGIN = 8;

{ TAdvSignatureCapture }

procedure TAdvSignatureCapture.WriteStringToStream(const Stream: TStream; const s: string);
var
  n: LongInt;
begin
  n := Length(s);
  Stream.Write(n, SizeOf(n));
  if n > 0 then Stream.Write(s[1], n * sizeof(char));
end;

function TAdvSignatureCapture.ReadStringFromStream(const Stream: TStream): string;
var
  n: LongInt;
begin
  Stream.Read(n, SizeOf(n));
  SetLength(Result, n);
  if n > 0 then Stream.Read(Result[1], n * sizeof(char));
end;

procedure TAdvSignatureCapture.Clear;
var
  i: Integer;
begin
  for i := 0 to FSignaturePoints.Count - 1 do
     FSignaturePoints[i].Free;
  FSignaturePoints.Clear;
  Invalidate;
end;

procedure TAdvSignatureCapture.ClearPosChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvSignatureCapture.CMColorChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TAdvSignatureCapture.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

constructor TAdvSignatureCapture.Create(AOwner: TComponent);
begin
  inherited;
  Width := 400;
  Height := 300;
  FSignaturePoints := TList<TStoredPoint>.Create;
  FPen := TPen.Create;
  FPen.OnChange := PenChanged;
  FDrawing := False;
  Color := clWhite;
  FText := 'Sign here.';
  FBorderStyle := bsSingle;
  FBorderColor := clGray;
  FTextPos := TTextPosition.Create;
  FTextPos.OnChange := TextPosChanged;
  FClearSig := TClearPosition.Create;
  FClearSig.OnChange := ClearPosChanged;
end;

destructor TAdvSignatureCapture.Destroy;
begin
  FPen.Free;
  FTextPos.Free;
  FClearSig.Free;
  Clear;
  FSignaturePoints.Free;
  inherited;
end;

procedure TAdvSignatureCapture.DoPaint(const ACanvas: TCanvas;
  const ARect: TRect);
begin
  if Assigned(OnPaint) then
    OnPaint(Self, ACanvas, ARect);
end;

procedure TAdvSignatureCapture.DrawSignature;
var
  i: Integer;
  drawPoint: TStoredPoint;
begin
  for i := 0 to FSignaturePoints.Count -1 do
  begin
    drawPoint := FSignaturePoints.Items[i];
    if drawPoint.DrawLine then
    begin
      Canvas.Pen.Width := drawPoint.PenWidth;
      Canvas.Pen.Color := drawPoint.PenColor;
      Canvas.LineTo(drawpoint.X, drawPoint.Y);
    end
    else
      Canvas.MoveTo(drawPoint.X, drawPoint.Y);
  end;
end;

function TAdvSignatureCapture.GetClearRect: TRect;
var
  w,h: integer;
  cr: TRect;
  c: integer;
begin
  Result := Rect(0,0,0,0);

  if ClearSig.Picture.Graphic.Empty then
    Exit;

  w := ClearSig.Picture.Width;
  h := ClearSig.Picture.Height;

  cr := ClientRect;

  if TextPos.Position in [pTopCenter, pTopLeft, pTopRight] then
  begin
    Canvas.Font.Assign(Font);
    cr.Top := Canvas.TextHeight('gh') + TEXTMARGIN + TextPos.LineWidth + CLEARMARGIN;
  end;

  if TextPos.Position in [pBottomCenter, pBottomLeft, pBottomRight] then
  begin
    Canvas.Font.Assign(Self.Font);
    cr.Bottom := cr.Bottom - (Canvas.TextHeight('gh') + TEXTMARGIN + TextPos.LineWidth + CLEARMARGIN);
  end;

  c := (cr.Right - cr.Left) div 2;

  case ClearSig.Position of
    pBottomCenter: Result := Rect( c - (w div 2), cr.Bottom - h, c + (w div 2), cr.Bottom);
    pBottomLeft: Result := Rect(cr.Left + CLEARMARGIN, cr.Bottom - h, cr.Left + CLEARMARGIN + w, cr.Bottom);
    pBottomRight: Result := Rect(cr.Right - w - CLEARMARGIN, cr.Bottom - h, cr.Right - CLEARMARGIN, cr.Bottom);
    pTopCenter: Result := Rect( c - (w div 2), cr.Top, c + (w div 2), cr.Top + h);
    pTopLeft: Result := Rect(cr.Left + CLEARMARGIN, cr.Top, cr.Left + CLEARMARGIN + w, cr.Top + h);
    pTopRight: Result := Rect(cr.Right - w - CLEARMARGIN, cr.Top, cr.Right - CLEARMARGIN, cr.Top + h);
    pXY: Result := Rect(ClearSig.x, ClearSig.y, ClearSig.x + w, ClearSig.y + h);
    pNone: ;
  end;


end;

function TAdvSignatureCapture.GetColorEx: TColor;
begin
  Result := inherited Color;
end;

function TAdvSignatureCapture.GetEmpty: boolean;
begin
  Result := FSignaturePoints.Count = 0;
end;

function TAdvSignatureCapture.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvSignatureCapture.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvSignatureCapture.LoadFromFile(FileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(FileName);
    ms.Position := 0;
    LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TAdvSignatureCapture.LoadFromStream(AStream: TStream);
var
  i, sizeTest: Integer;
  point: TStoredPoint;
  x,y,w:integer;
  c: TColor;
  d: boolean;
  sTest: string;
begin
  sizeTest := Random(50);
  AStream.Position := 0;
  AStream.Read(c, SizeOf(c));
  Color := c;

  AStream.Read(sizeTest, SizeOf(sizeTest));
  Clear;
  for I := 0 to sizeTest -1 do
  begin
    point := TStoredPoint.Create();

    AStream.Read(X, SizeOf(X));
    AStream.Read(Y, SizeOf(Y));
    AStream.Read(W, SizeOf(W));
    AStream.Read(C, SizeOf(TColor));
    AStream.Read(D, SizeOf(D));

    point.X := X;
    point.Y := Y;
    point.PenWidth := w;
    point.PenColor := c;
    point.DrawLine := d;

    FSignaturePoints.Add(point);
  end;
  FText := ReadStringFromStream(AStream);
  AStream.Read(i, SizeOf(i));
  Font.Size := i;
  AStream.Read(c, SizeOf(TColor));
  Font.Color := c;
  AStream.Read(i, SizeOf(Integer));
  Font.Style := TFontStyles(byte(i));
  sTest := ReadStringFromStream(AStream);
  Font.Name := TFontName(sTest);
  Invalidate;
end;

procedure TAdvSignatureCapture.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  newPoint: TStoredPoint;
begin
  inherited;


  if (Button = mbLeft) then
  begin
    FClearDown := PtInRect(GetClearRect,Point(X,Y));

    if not FClearDown then
    begin
      FDrawing := True;
      Canvas.MoveTo(X, Y);

      newPoint := TStoredPoint.Create;
      newPoint.X := X;
      newPoint.Y := Y;
      newPoint.DrawLine := False;
      newPoint.PenWidth := FPen.Width;
      newPoint.PenColor := FPen.Color;
      FSignaturePoints.Add(newPoint);
    end;
  end;
end;

procedure TAdvSignatureCapture.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  newPoint: TStoredPoint;
begin
  inherited;

  if FDrawing then
  begin
    Canvas.LineTo(X,Y);
    newPoint := TStoredPoint.Create;
    newPoint.X := X;
    newPoint.Y := Y;
    newPoint.DrawLine := True;
    newPoint.PenWidth := FPen.Width;
    newPoint.PenColor := FPen.Color;
    FSignaturePoints.Add(newPoint);
  end;
end;

procedure TAdvSignatureCapture.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  newPoint: TStoredPoint;
begin
  inherited;

  if (Button = mbLeft) then
  begin
    if FClearDown and PtInRect(GetClearRect,Point(X,Y)) then
    begin
      Clear;
    end
    else
    begin
      Canvas.LineTo(X,Y);
      FDrawing := False;
      newPoint := TStoredPoint.Create;
      newPoint.X := X;
      newPoint.Y := Y;
      newPoint.DrawLine := True;
      newPoint.PenWidth := FPen.Width;
      newPoint.PenColor := FPen.Color;
      FSignaturePoints.Add(newPoint);
    end;
  end;
end;

procedure TAdvSignatureCapture.Paint;
var
  cr,dr,ccr: TRect;
  th: integer;
  DTS: DWORD;
begin
  inherited;
  cr := ClientRect;
  dr := cr;
  ccr := cr;

  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := Color;

  if FBorderStyle = bsSingle then
  begin
    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Width := 1;
  end;

  Canvas.Rectangle(cr);
  Canvas.Font := Font;

  th := Canvas.TextHeight('gh') + TEXTMARGIN;

  case TextPos.Position of
    pBottomCenter, pBottomLeft, pBottomRight:
      begin
        if (TextPos.LineWidth > 0) and (TextPos.LineColor <> clNone) then
        begin
          Canvas.Pen.Color := TextPos.LineColor;
          Canvas.Pen.Width := TextPos.LineWidth;
          Canvas.MoveTo(0, cr.Bottom - th);
          Canvas.LineTo(cr.Right, cr.Bottom - th);
        end;
        dr.Top := cr.Bottom - th;
        ccr.Bottom := dr.Top;
      end;
    pTopCenter, pTopLeft, pTopRight:
      begin
        if (TextPos.LineWidth > 0) and (TextPos.LineColor <> clNone) then
        begin
          Canvas.Pen.Color := TextPos.LineColor;
          Canvas.Pen.Width := TextPos.LineWidth;
          Canvas.MoveTo(0, th);
          Canvas.LineTo(cr.Right, th);
        end;
        dr.Bottom := cr.Top + th;
        ccr.Top := dr.Bottom;
      end;
  end;

  DTS := DT_SINGLELINE or DT_VCENTER;

  case TextPos.Position of
  pBottomCenter, pTopCenter: DTS := DTS OR DT_CENTER;
  pBottomLeft, pTopLeft: DTS := DTS OR DT_LEFT;
  pBottomRight, pTopRight: DTS := DTS OR DT_RIGHT;
  end;

  if TextPos.Position = pXY then
    Canvas.TextOut(FTextPos.X, FTextPos.Y, FText)
  else
    if TextPos.Position <> pNone then
    begin
      dr.Left := dr.Left + 2;
      dr.Right := dr.Right - 2;
      DrawText(Canvas.Handle, PChar(Text), Length(Text), dr, DTS);
    end;


  if Assigned(ClearSig.Picture.Graphic) and not ClearSig.Picture.Graphic.Empty and not (ClearSig.Position = pNone) then
  begin
    ccr := GetClearRect;
    Canvas.Draw(ccr.Left, ccr.Top, ClearSig.Picture.Graphic);
  end;

  Canvas.Pen.Assign(FPen);

  DrawSignature;

  DoPaint(Canvas, cr);
end;

procedure TAdvSignatureCapture.PenChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvSignatureCapture.SaveToFile(FileName: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    SaveToStream(ms);
    ms.SaveToFile(FileName);
  finally
    ms.Free;
  end;
end;

procedure TAdvSignatureCapture.SaveToImageFile(FileName: string);
var
  img: TGraphic;
  rectangle: TRect;
  bmp: TBitmap;
  ext: string;
begin
  img := nil;

  ext := Uppercase(ExtractFileExt(FileName));

  if (ext = '.JPG') or (ext = '.JPEG') then
    img := TJPEGImage.Create;
  if (ext = '.BMP') then
    img := TBitmap.Create;
  if (ext = '.PNG') then
   img := TPngImage.Create;

  bmp := TBitmap.Create;
  try
    bmp.Width := Width;
    bmp.Height := Height;
    rectangle := Rect(0,0,Width,Height);
    bmp.Canvas.CopyRect(rectangle, Canvas, rectangle);
    img.Assign(bmp);
    img.SaveToFile(FileName);
  finally
    bmp.Free;
    if Assigned(img) then
      img.Free;
  end;
end;

procedure TAdvSignatureCapture.SaveToStream(AStream: TStream);
var
  i: Integer;
  point: TStoredPoint;
  c: TColor;
begin
  AStream.Position := 0;
  c := Color;
  AStream.Write(c, SizeOf(c));
  AStream.Write(FSignaturePoints.Count, SizeOf(FSignaturePoints.Count));

  for i := 0 to FSignaturePoints.Count - 1 do
  begin
    point := FSignaturePoints.Items[i];
    AStream.Write(point.X, SizeOf(point.X));
    AStream.Write(point.Y, SizeOf(point.Y));
    AStream.Write(point.PenWidth, SizeOf(point.PenWidth));
    AStream.Write(point.PenColor, SizeOf(point.PenColor));
    AStream.Write(point.DrawLine, SizeOf(point.DrawLine));
  end;
  WriteStringToStream(AStream, FText);
  i:= Font.Size;
  AStream.Write(i, SizeOf(i));
  AStream.Write(Font.Color, SizeOf(Font.Color));
  i := byte(Font.Style);
  AStream.Write(i, SizeOf(i));
  WriteStringToStream(AStream, Font.Name);
end;


procedure TAdvSignatureCapture.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

procedure TAdvSignatureCapture.SetBorderStyle(const Value: TBorderStyle);
begin
  FBorderStyle := Value;
  Invalidate;
end;

procedure TAdvSignatureCapture.SetClearSig(const Value: TClearPosition);
begin
  FClearSig.Assign(Value);
end;

procedure TAdvSignatureCapture.SetColorEx(const Value: TColor);
begin
  inherited Color := Value;
end;

procedure TAdvSignatureCapture.SetEmpty(const Value: boolean);
begin
  if Value then
    Clear;
end;

procedure TAdvSignatureCapture.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
  Invalidate;
end;

procedure TAdvSignatureCapture.SetText(const Value: string);
begin
  FText := Value;
  Invalidate;
end;

procedure TAdvSignatureCapture.SetTextPos(const Value: TTextPosition);
begin
  FTextPos.Assign(Value);
  Invalidate;
end;

procedure TAdvSignatureCapture.SetVersion(const Value: string);
begin

end;

procedure TAdvSignatureCapture.TextPosChanged(Sender: TObject);
begin
  Invalidate;
end;

{ TTextPosition }

procedure TTextPosition.Assign(Source: TPersistent);
begin
  if (Source is TTextPosition) then
  begin
    FX := (Source as TTextPosition).X;
    FY := (Source as TTextPosition).Y;
    FLineColor := (Source as TTextPosition).LineColor;
    FLineWidth := (Source as TTextPosition).LineWidth;
    FPosition := (Source as TTextPosition).Position;
  end;
end;

procedure TTextPosition.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TTextPosition.Create;
begin
  inherited;
  FX := 25;
  FY := 25;
  FPosition := pBottomCenter;
  FLineColor := clGray;
  FLineWidth := 2;
end;

procedure TTextPosition.SetLineColor(const Value: TColor);
begin
  if (FLineColor <> Value) then
  begin
    FLineColor := Value;
    Changed;
  end;
end;

procedure TTextPosition.SetLineWidth(const Value: integer);
begin
  if (FLineWidth <> Value) then
  begin
    FLineWidth := Value;
    Changed;
  end;
end;

procedure TTextPosition.SetPosition(const Value: TTextHintPosition);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TTextPosition.SetX(const Value: integer);
begin
  if (FX <> Value) then
  begin
    FX := Value;
    Changed;
  end;
end;

procedure TTextPosition.SetY(const Value: integer);
begin
  if (FY <> Value) then
  begin
    FY := Value;
    Changed;
  end;
end;

{ TClearPosition }

procedure TClearPosition.Assign(Source: TPersistent);
begin
  if (Source is TClearPosition) then
  begin
    FX := (Source as TClearPosition).X;
    FY := (Source as TClearPosition).Y;
    FPicture.Assign((Source as TClearPosition).Picture);
    FPosition := (Source as TClearPosition).Position;
  end;
end;

procedure TClearPosition.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TClearPosition.Create;
var
  RS: TResourceStream;
  PNGImage: TPNGImage;

begin
  inherited;
  FPicture := TPicture.Create;
  FPosition := pBottomLeft;

  PNGImage := TPNGImage.Create;
  try
    RS := TResourceStream.Create(hInstance, 'tmscancelsig', RT_RCDATA);
    try
      PNGImage.LoadFromStream(RS);
      FPicture.Graphic := PNGImage;
    finally
      RS.Free;
    end;
  finally
    PNGImage.Free;
  end;
end;

destructor TClearPosition.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TClearPosition.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TClearPosition.SetPosition(const Value: TTextHintPosition);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TClearPosition.SetX(const Value: integer);
begin
  if (FX <> Value) then
  begin
    FX := Value;
    Changed;
  end;
end;

procedure TClearPosition.SetY(const Value: integer);
begin
  if (FY <> Value) then
  begin
    FY := Value;
    Changed;
  end;
end;

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}

end.
