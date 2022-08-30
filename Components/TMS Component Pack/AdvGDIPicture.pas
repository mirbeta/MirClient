{***************************************************************************}
{ TAdvGDIPicture                                                            }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by                                                                }
{   TMS Software                                                            }
{   copyright © 2006 - 2015                                                 }
{   Email : info@tmssoftware.com                                            }
{   Web : http://www.tmssoftware.com                                        }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvGDIPicture;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, Classes, Graphics, Controls , SysUtils, AdvGDIP, GDIPicture,
  ComObj, ActiveX, ExtDlgs, Forms, Types;

{$R AdvGDIPicture.res}

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New: Added property PopupMenu
  //          : Fixed: issue with Proportional=true
  // v1.0.1.1 : Fixed: issue with centering
  // v1.0.2.0 : New : changes for allow use of DB-aware version
  // v1.1.0.0 : New : Cropping property added
  //          : New : OnMouseLeave/OnMouseEnter exposed
  // v1.2.0.0 : New : StretchMode property added
  // v1.3.0.0 : New : EmptyText property added
  //          : New : AllowSelectOnClick property added
  //          : New : BorderStyle, BorderColor, BorderPenStyle properties added
  // v1.3.0.1 : Fixed : Issue with specific stretching with aspect ratio
  // v1.4.0.0 : New : Built-in optional clear button
  //          : New : Built-in optional select button
  // v1.4.0.1 : Fixed : Issue with erratically clearing picture
  // v1.4.1.0 : New : Property TransparentBitmap added
  // v1.4.2.0 : New : method PaintTo() added
  // v1.4.3.0 : New : Public property Angle added


type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TStretchMode = (smAlways, smStretchOnly, smShrinkOnly);

  TGDIPImageFileType = (gtJPEG,gtBMP,gtPNG,gtGIF,gtEMF,gtAll,gtICO);
  TGDIPImageFileTypes = set of TGDIPImageFileType;

  TGDIPBorderStyle = (bsDesign, bsAlways, bsEmpty, bsNever);

  TGDIPPictureSelected = procedure(Sender: TObject; FileName: string) of object;

  TGDIPPictureSelect = procedure(Sender: TObject; FileName: string; var Allow: boolean) of object;

  TButtonPosition = (bpNone, bpTopLeft, bpTopRight, bpBottomLeft, bpBottomRight);

  TButtonShow = (sHover, sAlways);

  TPictureButton = class(TPersistent)
  private
    FHint: string;
    FTextColor: TColor;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FSize: integer;
    FPosition: TButtonPosition;
    FShow: TButtonShow;
    procedure SetColor(const Value: TColor);
    procedure SetPosition(const Value: TButtonPosition);
    procedure SetSize(const Value: integer);
    procedure SetTextColor(const Value: TColor);
    procedure SetShow(const Value: TButtonShow);

  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property TextColor: TColor read FTextColor write SetTextColor default clWindowText;
    property Hint: string read FHint write FHint;
    property Size: integer read FSize write SetSize default 20;
    property Show: TButtonShow read FShow write SetShow default sHover;
    property Position: TButtonPosition read FPosition write SetPosition default bpNone;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGDIPPicture = class(TGraphicControl)
  private
    { Private declarations }
    FAutoSize: Boolean;
    FIPicture: TGDIPPicture;
    FDoubleBuffered: Boolean;
    FBackgroundColor: TColor;
    FRefresh: Boolean;
    FCenter: Boolean;
    FStretch: Boolean;
    FProportional: Boolean;
    FCropping: Boolean;
    FStrechMode: TStretchMode;
    FAllowSelectOnClick: boolean;
    FImageTypes: TGDIPImageFileTypes;
    FBorderColor: TColor;
    FBorderStyle: TGDIPBorderStyle;
    FBorderPenStyle: TPenStyle;
    FEmptyText: string;
    FOnPictureSelected: TGDIPPictureSelected;
    FOnPictureSelect: TGDIPPictureSelect;
    FButtonClear: TPictureButton;
    FButtonSelect: TPictureButton;
    FClearDown: boolean;
    FClearHover: boolean;
    FSelectDown: boolean;
    FSelectHover: boolean;
    FOnPictureClear: TNotifyEvent;
    FTransparentBitmap: boolean;
    FAngle: Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure SetProportional(const Value: Boolean);
    procedure SetStretch(const Value: Boolean);
    procedure SetCenter(const Value: Boolean);
    procedure SetAutoSizeP(const Value: Boolean);
    procedure SetGDIPPicture(const Value: TGDIPPicture);
    procedure PictureChanged(Sender:TObject);
    procedure PictureCleared(Sender:TObject);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetDoubleBuffered(const Value: Boolean);
    function GetVersion: string;
    function GetVersionNr: Integer;
    procedure SetCropping(const Value: Boolean);
    procedure SetStretchMode(const Value: TStretchMode);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderStyle(const Value: TGDIPBorderStyle);
    procedure SetBorderPenStyle(const Value: TPenStyle);
    procedure SetEmptyText(const Value: string);
    procedure SetButtonClear(const Value: TPictureButton);
    procedure SetButtonSelect(const Value: TPictureButton);
    procedure SetVersion(const Value: string);
    procedure SetTransparentBitmap(const Value: boolean);
    procedure SetAngle(const Value: Integer);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure Click; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PictureChange; virtual;
    procedure DoPictureClear; virtual;
    procedure DoPictureSelect(FileName: string; var Allow: boolean); virtual;
    procedure DoPictureSelected(FileName: string); virtual;
    procedure ButtonChanged(Sender: TObject);
    function GetClearRect: TRect;
    function GetSelectRect: TRect;
    function InClearRect(X,Y: integer): boolean;
    function InSelectRect(X,Y: integer): boolean;
    function ShouldShowButtonClear: boolean;
    function ShouldShowButtonSelect: boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure PaintTo(ACanvas: TCanvas; DrawRect: TRect);
    property DoubleBuffered: Boolean read FDoubleBuffered write SetDoubleBuffered;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property Angle: Integer read FAngle write SetAngle;
    procedure SelectPicture;
  published
    { Published declarations }
    property AutoSize: Boolean read FAutoSize write SetAutoSizeP default False;
    property AllowSelectOnClick: boolean read FAllowSelectOnClick write FAllowSelectOnClick default false;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderStyle: TGDIPBorderStyle read FBorderStyle write SetBorderStyle default bsDesign;
    property BorderPenStyle: TPenStyle read FBorderPenStyle write SetBorderPenStyle default psDashDot;
    property ButtonClear: TPictureButton read FButtonClear write SetButtonClear;
    property ButtonSelect: TPictureButton read FButtonSelect write SetButtonSelect;
    property Center: Boolean read FCenter write SetCenter default False;
    property Cropping: Boolean read FCropping write SetCropping default False;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property Picture: TGDIPPicture read FIPicture write SeTGDIPPicture;
    property Proportional: Boolean read FProportional write SetProportional default False;
    property ImageTypes: TGDIPImageFileTypes read FImageTypes write FImageTypes;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property StretchMode: TStretchMode read FStrechMode write SetStretchMode default smAlways;
    property TransparentBitmap: boolean read FTransparentBitmap write SetTransparentBitmap default true;
    { inherited published properties}
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnStartDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    {$IFDEF DELPHI2005_LVL}
    property OnMouseLeave;
    property OnMouseEnter;
    {$ENDIF}
    property OnStartDrag;
    property OnPictureClear: TNotifyEvent read FOnPictureClear write FOnPictureClear;
    property OnPictureSelect: TGDIPPictureSelect read FOnPictureSelect write FOnPictureSelect;
    property OnPictureSelected: TGDIPPictureSelected read FOnPictureSelected write FOnPictureSelected;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

uses
  Math;

{ TAdvGDIPPicture }

procedure TAdvGDIPPicture.ButtonChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvGDIPPicture.Click;
begin
  inherited;
end;

procedure TAdvGDIPPicture.CMHintShow(var Msg: TMessage);
var
  hi: PHintInfo;
begin
  inherited;

  hi := PHintInfo(Msg.LParam);

  if InClearRect(hi.CursorPos.X, hi.CursorPos.Y) then
    hi.HintStr := ButtonClear.Hint;

  if InSelectRect(hi.CursorPos.X, hi.CursorPos.Y) then
    hi.HintStr := ButtonSelect.Hint;
end;

procedure TAdvGDIPPicture.CMMouseLeave(var Message: TMessage);
begin
  if FClearHover or FSelectHover then
  begin
    FClearHover := false;
    FSelectHover := false;
    Invalidate;
  end;
end;

constructor TAdvGDIPPicture.Create(AOwner: TComponent);
begin
  inherited;
  FIPicture := TGDIPPicture.Create;
  FIPicture.OnChange := PictureChanged;
  FIPicture.OnClear := PictureCleared;
  Width := 100;
  Height := 100;
  FCropping := False;
  FBorderColor := clBlack;
  FBorderStyle := bsDesign;
  FBorderPenStyle := psDashDot;

  FButtonClear := TPictureButton.Create;
  FButtonClear.OnChange := ButtonChanged;

  FButtonSelect := TPictureButton.Create;
  FButtonSelect.OnChange := ButtonChanged;

  FDoubleBuffered := true;
  FTransparentBitmap := true;
end;

destructor TAdvGDIPPicture.Destroy;
begin
  FButtonClear.Free;
  FButtonSelect.Free;
  FIPicture.Free;
  FIPicture := nil;
  inherited;
end;

procedure TAdvGDIPPicture.DoPictureClear;
begin
  if Assigned(OnPictureClear) then
    OnPictureClear(Self);
end;

procedure TAdvGDIPPicture.DoPictureSelect(FileName: string; var Allow: boolean);
begin
  if Assigned(OnPictureSelect) then
    OnPictureSelect(Self,FileName, Allow);
end;

procedure TAdvGDIPPicture.DoPictureSelected(FileName: string);
begin
  if Assigned(OnPictureSelected) then
    OnPictureSelected(Self,FileName);
end;

procedure TAdvGDIPPicture.Loaded;
begin
  inherited;
end;

procedure TAdvGDIPPicture.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if InClearRect(X,Y) then
    FClearDown := true;

  if InSelectRect(X,Y) then
    FSelectDown := true;
end;

procedure TAdvGDIPPicture.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if InClearRect(X,Y) then
  begin
    if not FClearHover then
    begin
      FClearHover := true;
      Invalidate;
    end;
  end
  else
  begin
    if FClearHover then
    begin
      FClearHover := false;
      Invalidate;
    end;
  end;

  if InSelectRect(X,Y) then
  begin
    if not FSelectHover then
    begin
      FSelectHover := true;
      Invalidate;
    end;
  end
  else
  begin
    if FSelectHover then
    begin
      FSelectHover := false;
      Invalidate;
    end;
  end;
end;

procedure TAdvGDIPPicture.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if InClearRect(X,Y) and FClearDown then
  begin
    Picture := nil;
    DoPictureClear;
  end;

  if InSelectRect(X,Y) and FSelectDown and AllowSelectOnClick then
    SelectPicture;

  FClearDown := false;
  FSelectDown := false;
end;

procedure TAdvGDIPPicture.Paint;
var
  h: integer;
  r: TRect;
  s: string;
begin
  PaintTo(Canvas, ClientRect);

  if ((csDesigning in ComponentState) and (BorderStyle <> bsNever)) or (BorderStyle <> bsDesign) then
  begin
    if (BorderStyle = bsAlways) or ((BorderStyle = bsEmpty) and Picture.Empty) then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := BorderPenStyle;
      Canvas.Pen.Width := 1;
      Canvas.Pen.Color := BorderColor;
      Canvas.Rectangle(ClientRect);
    end;
  end;

  if (EmptyText <> '') and (Picture.Empty) then
  begin
    r := ClientRect;
    InflateRect(r, -4, -4);
    Canvas.Font.Assign(Font);

    h := DrawText(Canvas.Handle, PChar(EmptyText), Length(EmptyText),r,DT_CENTER or DT_WORDBREAK or DT_CALCRECT);

    r := ClientRect;
    r.Top := r.Top + (Height - h) div 2;
    InflateRect(r, -4, -4);

    DrawText(Canvas.Handle, PChar(EmptyText), Length(EmptyText),r,DT_CENTER or DT_WORDBREAK);
  end;

  if ShouldShowButtonClear then
  begin
    r := GetClearRect;

    if ButtonClear.Color <> clNone then
    begin
      Canvas.Brush.Color := ButtonClear.Color;
      Canvas.Pen.Color := ButtonClear.Color;
      Canvas.FillRect(r);
    end;
    if ButtonClear.TextColor <> clNone then
    begin
      Canvas.Font.Name := 'Webdings';
      Canvas.Font.Size := 8;
      Canvas.Font.Color := ButtonClear.TextColor;

      s := chr($72);
      DrawText(Canvas.Handle,PChar(s),1,r,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end;
  end;

  if ShouldShowButtonSelect then
  begin
    r := GetSelectRect;

    if ButtonSelect.Color <> clNone then
    begin
      Canvas.Brush.Color := ButtonSelect.Color;
      Canvas.Pen.Color := ButtonSelect.Color;
      Canvas.FillRect(r);
    end;
    if ButtonSelect.TextColor <> clNone then
    begin
      Canvas.Font.Name := 'Wingdings';
      Canvas.Font.Size := 8;
      Canvas.Font.Color := ButtonSelect.TextColor;

      s := chr($31);
      DrawText(Canvas.Handle,PChar(s),1,r,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
    end;
  end;
end;

procedure TAdvGDIPPicture.PaintTo(ACanvas: TCanvas; DrawRect: TRect);
var
  w,h,pw,ph,x,y: integer;
  rw,rh: double;
  r: TRect;
  ar, arc: double;
  DWidth, DHeight: integer;

begin
  DWidth := DrawRect.Right - DrawRect.Left;
  DHeight := DrawRect.Bottom - DrawRect.Top;

  FIPicture.Angle := Angle;

  if Stretch or Cropping then
  begin
    FIPicture.GetImageSizes;
    FIPicture.TransparentBitmap := TransparentBitmap;
    pw := FIPicture.Width;
    ph := FIPicture.Height;

    if (Angle <> 0) then
    begin
      rw := pw * Cos(DegToRad(Angle)) + ph * Sin(DegToRad(Angle));
      rh := pw * Sin(DegToRad(Angle)) + ph * Cos(DegToRad(Angle));
      pw := Round(rw);
      ph := Round(rh);
    end;

    w := pw;
    h := ph;

    if Proportional or Cropping then
    begin
      if (w >= h) and (w > 0) then
      begin
        h := Round(DWidth / w * h);
        w := DWidth;

        if (h > DHeight) then
        begin
          w := Round(DHeight / h * w);
          h := DHeight;
        end;
      end
      else
      if (h >= w) and (h > 0) then
      begin
        w := Round(DHeight / h * w);
        h := DHeight;

        if (w > DWidth) then
        begin
          h := Round(DWidth / w * h);
          w := DWidth;
        end;
      end;

      x := 0;
      y := 0;

      if h = 0 then
        ar := 1
      else
        ar := w/h;

      if DHeight = 0 then
        arc := 1
      else
        arc := DWidth / DHeight;

      if Cropping then
      begin
        if (ar < 1) or (arc > ar) then
        begin
          h := Round(DWidth / ar);
          w := DWidth;
        end
        else
        begin
          w := Round(ar * DHeight);
          h := DHeight;
        end;
      end;

      if Center or Cropping then
      begin
//        if (w < Width) then
          x := (DWidth - w) div 2;

//        if (h < Height) then
          y := (DHeight - h) div 2;
      end;



      if Stretch then
      begin
        if StretchMode = smShrinkOnly then
        begin
          if (pw < DWidth) and (ph < DHeight) then
          begin
            w := pw;
            h := ph;
          end;
        end;

        if StretchMode = smStretchOnly then
        begin
          if (pw > DWidth) or (ph > DHeight) then
          begin
            h := ph;
            w := pw;
          end
          else
          begin
            if (pw < DWidth) then
            begin
              w := DWidth;
              h := round(w / ar);
            end;

            if (ph < DHeight) then
            begin
              h := DHeight;
              w := round(h * ar);
            end;
          end;
        end;
      end;

      r := Rect(x, y, x + w, y + h);

      FIPicture.Draw(ACanvas, r);
    end
    else
    begin
      r := DrawRect;
      x := 0;
      y := 0;
      if Stretch then
      begin
        if StretchMode = smShrinkOnly then
        begin
          if (pw > DWidth) then
            w := DWidth;
          if (ph > DHeight) then
            h := DHeight;
          r := Rect(x, y, x + w, y + h);
        end;

        if StretchMode = smStretchOnly then
        begin
          if (pw < DWidth) then
            w := DWidth;

          if (ph < DHeight) then
            h := DHeight;

          r := Rect(x, y, x + w, y + h);
        end;
      end;

      FIPicture.Draw(ACanvas, r);
    end;
  end
  else
  begin
    if Center then
    begin
      FIPicture.GetImageSizes;
      w := FIPicture.Width;
      h := FIPicture.Height;

//      pw := w;
//      ph := h;
//
//      if (Angle <> 0) then
//      begin
//        rw := w * Cos(DegToRad(Angle)) + h * Sin(DegToRad(Angle));
//        rh := w * Sin(DegToRad(Angle)) + h * Cos(DegToRad(Angle));
//        pw := Round(rw);
//        ph := Round(rh);
//      end;

      x := 0;
      y := 0;

      if (w < DWidth) then
        x := (DWidth - w) div 2;

      if (h < DHeight) then
        y := (DHeight - h) div 2;

//      if Angle <> 0 then
//      begin
//        x := x - (pw - w) div 2;
//        y := y + (ph - h) div 2;
//      end;

      R := Rect(x,y,x + w,y + h);

      FIPicture.Draw(ACanvas, r);
    end
    else
    begin
      FIPicture.GetImageSizes;
      w := FIPicture.Width;
      h := FIPicture.Height;


      R := Rect(0,0,w,h);

      FIPicture.Draw(ACanvas, r);
    end;
  end;
end;

procedure TAdvGDIPPicture.PictureChange;
begin

end;

procedure TAdvGDIPPicture.PictureChanged(sender: TObject);
begin
  if FAutoSize and not FIPicture.Empty then
    SetAutoSizeP(FAutoSize);

  PictureChange;
  Invalidate;
end;

procedure TAdvGDIPPicture.SelectPicture;
var
  pd: TOpenPictureDialog;
  fltr: string;
  ext: string;
  Allow: boolean;

  function AppendStr(res, s, split: string): string;
  begin
    if res = '' then
      Result := s
    else
      Result := res + split + s;
  end;

begin
  pd := TOpenPictureDialog.Create(Self);

  fltr := '';
  ext := '';

  if gtJPEG in ImageTypes then
  begin
    fltr := AppendStr(fltr,'JPG Images (*.jpg)|*.jpg|JPEG Images (*.jpeg)|*.jpeg','|');
    ext := AppendStr(ext,'*.jpg;*.jpeg',';');
  end;

  if gtGIF in ImageTypes then
  begin
    fltr := AppendStr(fltr,'GIF Images (*.gif)|*.gif','|');
    ext := AppendStr(ext,'*.gif',';');
  end;

  if gtBMP in ImageTypes then
  begin
    fltr := AppendStr(fltr,'Bitmap images (*.bmp)|*.bmp','|');
    ext := AppendStr(ext,'*.bmp',';');
  end;

  if gtPNG in ImageTypes then
  begin
    fltr := AppendStr(fltr,'PNG images (*.png)|*.png','|');
    ext := AppendStr(ext,'*.png',';');
  end;

  if gtICO in ImageTypes then
  begin
    fltr := AppendStr(fltr,'Icons (*.ico)|*.ico','|');
    ext := AppendStr(ext,'*.ico',';');
  end;

  if gtEMF in ImageTypes then
  begin
    fltr := AppendStr(fltr,'Enhanced metafiles (*.emf)|*.emf','|');
    ext := AppendStr(ext,'*.emf',';');
  end;

  if (gtAll in ImageTypes) and (fltr <> '') then
  begin
    fltr := 'All (' + ext + ')|'+ext+'|'+fltr;
  end;

  try
    pd.Filter := fltr;
    if pd.Execute then
    begin
      Allow := true;
      DoPictureSelect(pd.FileName, Allow);
      if Allow then
      begin
        Picture.LoadFromFile(pd.FileName);
        DoPictureSelected(pd.FileName);
      end;
    end;
  finally
    pd.Free;
  end;
end;

procedure TAdvGDIPPicture.SetAngle(const Value: Integer);
begin
  FAngle := Value;
  Repaint;
end;

procedure TAdvGDIPPicture.SetAutoSizeP(const Value: Boolean);
var
  w,h: double;
begin
  FAutoSize := Value;
  if FAutoSize and not FIPicture.Empty then
  begin
    if FIPicture.GetImageSizes then
    begin
      Width := FIPicture.Width;
      Height := FIPicture.Height;

      outputdebugstring(pchar(inttostr(width)+';'+inttostr(height)));

      if (Angle <> 0) then
      begin
        w := Width * Cos(DegToRad(Angle)) + Height * Sin(DegToRad(Angle));
        h := Width * Sin(DegToRad(Angle)) + Height * Cos(DegToRad(Angle));
        Width := Abs(Round(w));
        Height := Abs(Round(h));

        outputdebugstring(pchar('R:'+inttostr(width)+';'+inttostr(height)));
      end;
    end;
  end;
end;

procedure TAdvGDIPPicture.SetGDIPPicture(const Value: TGDIPPicture);
begin
  FIPicture.Assign(Value);
  Invalidate;
end;

procedure TAdvGDIPPicture.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  FIPicture.BackgroundColor := Value;
end;

procedure TAdvGDIPPicture.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvGDIPPicture.SetBorderPenStyle(const Value: TPenStyle);
begin
  if (FBorderPenStyle <> Value) then
  begin
    FBorderPenStyle := Value;
    Invalidate;
  end;
end;

procedure TAdvGDIPPicture.SetBorderStyle(const Value: TGDIPBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TAdvGDIPPicture.SetButtonClear(const Value: TPictureButton);
begin
  FButtonClear.Assign(Value);
end;

procedure TAdvGDIPPicture.SetButtonSelect(const Value: TPictureButton);
begin
  FButtonSelect.Assign(Value);
end;

procedure TAdvGDIPPicture.SetCenter(const Value: Boolean);
begin
  if (FCenter <> Value) then
  begin
    FCenter := Value;
    Invalidate;
  end;
end;

procedure TAdvGDIPPicture.SetCropping(const Value: Boolean);
begin
  if (FCropping <> Value) then
  begin
    FCropping := Value;
    Changed;
  end;
end;

procedure TAdvGDIPPicture.SetDoubleBuffered(const Value: Boolean);
begin
  FDoubleBuffered := Value;
  FIPicture.DoubleBuffered := Value;
end;

procedure TAdvGDIPPicture.SetEmptyText(const Value: string);
begin
  if (FEmptyText <> Value) then
  begin
    FEmptyText := Value;
    Invalidate;
  end;
end;

procedure TAdvGDIPPicture.PictureCleared(Sender: TObject);
begin
  FRefresh := True;
  Repaint;
  FRefresh := False;
end;

function TAdvGDIPPicture.GetClearRect: TRect;
var
  r: TRect;
begin
  r := ClientRect;

  case ButtonClear.Position of
  bpTopLeft:
    begin
      r.Right := r.Left + ButtonClear.Size;
      r.Bottom := r.Top + ButtonClear.Size;
    end;
  bpTopRight:
    begin
      r.Left := r.Right - ButtonClear.Size;
      r.Bottom := r.Top + ButtonClear.Size;
    end;
  bpBottomLeft:
    begin
      r.Right := r.Left + ButtonClear.Size;
      r.Top := r.Bottom - ButtonClear.Size;
    end;
  bpBottomRight:
    begin
      r.Left := r.Right - ButtonClear.Size;
      r.Top := r.Bottom - ButtonClear.Size;
    end;
  end;

  Result := r;
end;

function TAdvGDIPPicture.GetSelectRect: TRect;
var
  r, br: TRect;
begin
  r := ClientRect;
  br := r;

  case ButtonClear.Position of
  bpTopLeft:
    begin
      r.Right := r.Left + ButtonClear.Size;
      if ButtonSelect.Position = bpTopLeft then
        br.Left := r.Right;
    end;
  bpTopRight:
    begin
      r.Left := r.Right - ButtonClear.Size;
      if ButtonSelect.Position = bpTopRight then
        br.Right := r.Left;
    end;
  bpBottomLeft:
    begin
      r.Right := r.Left + ButtonClear.Size;
      if ButtonSelect.Position = bpBottomLeft then
        br.Left := r.Right;
    end;
  bpBottomRight:
    begin
      r.Left := r.Right - ButtonClear.Size;
      if ButtonSelect.Position = bpBottomRight then
        br.Right := r.Left;
    end;
  end;

  case ButtonSelect.Position of
  bpTopLeft:
    begin
      br.Right := br.Left + ButtonSelect.Size;
      br.Bottom := br.Top + ButtonSelect.Size;
    end;
  bpTopRight:
    begin
      br.Left := br.Right - ButtonSelect.Size;
      br.Bottom := br.Top + ButtonSelect.Size;
    end;
  bpBottomLeft:
    begin
      br.Right := br.Left + ButtonSelect.Size;
      br.Top := br.Bottom - ButtonSelect.Size;
    end;
  bpBottomRight:
    begin
      br.Left := br.Right - ButtonSelect.Size;
      br.Top := br.Bottom - ButtonSelect.Size;
    end;
  end;

  Result := br;
end;

function TAdvGDIPPicture.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvGDIPPicture.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvGDIPPicture.InClearRect(X, Y: integer): boolean;
begin
  Result := PtInRect(GetClearRect,Point(X,Y)) and (ButtonClear.Position <> bpNone);
end;

function TAdvGDIPPicture.InSelectRect(X, Y: integer): boolean;
begin
  Result := PtInRect(GetSelectRect,Point(X,Y)) and (ButtonSelect.Position <> bpNone);
end;

procedure TAdvGDIPPicture.SetProportional(const Value: Boolean);
begin
  if (FProportional <> Value) then
  begin
    FProportional := Value;
    Invalidate;
  end;
end;

procedure TAdvGDIPPicture.SetStretch(const Value: Boolean);
begin
  if (FStretch <> Value) then
  begin
    FStretch := Value;
    Invalidate;
  end;
end;

procedure TAdvGDIPPicture.SetStretchMode(const Value: TStretchMode);
begin
  if (FStrechMode <> Value) then
  begin
    FStrechMode := Value;
    Invalidate;
  end;
end;

procedure TAdvGDIPPicture.SetTransparentBitmap(const Value: boolean);
begin
  if (FTransparentBitmap <> Value) then
  begin
    FTransparentBitmap := Value;
    Invalidate;
  end;
end;

procedure TAdvGDIPPicture.SetVersion(const Value: string);
begin

end;

function TAdvGDIPPicture.ShouldShowButtonClear: boolean;
begin
  Result := (ButtonClear.Position <> bpNone) and Enabled;
  Result := Result and ((csDesigning in ComponentState) or (ButtonClear.Show = sAlways) or FClearHover);
end;

function TAdvGDIPPicture.ShouldShowButtonSelect: boolean;
begin
  Result := (ButtonSelect.Position <> bpNone) and Enabled;
  Result := Result and ((csDesigning in ComponentState) or (ButtonSelect.Show = sAlways) or FSelectHover);
end;

{ TPictureButton }

procedure TPictureButton.Assign(Source: TPersistent);
begin
  if (Source is TPictureButton) then
  begin
    FColor := (Source as TPictureButton).Color;
    FTextColor := (Source as TPictureButton).TextColor;
    FSize := (Source as TPictureButton).Size;
    FHint := (Source as TPictureButton).Hint;
    FPosition := (Source as TPictureButton).Position;
    FShow := (Source as TPictureButton).Show;
  end;
end;

procedure TPictureButton.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TPictureButton.Create;
begin
  inherited;
  FSize := 20;
  FPosition := bpNone;
  FColor := clBtnFace;
  FTextColor := clWindowText;
  FShow := sHover;
end;

procedure TPictureButton.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TPictureButton.SetPosition(const Value: TButtonPosition);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TPictureButton.SetShow(const Value: TButtonShow);
begin
  FShow := Value;
end;

procedure TPictureButton.SetSize(const Value: integer);
begin
  if (FSize <> Value) then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TPictureButton.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed;
  end;
end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
