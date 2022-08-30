{*************************************************************************}
{ TMS Ribbon toolbars                                                     }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvToolBarExt;

interface

{$R AdvRibbonToolBar.res}

uses
  Classes, AdvToolBar, AdvGlowButton, ActnList, SysUtils, ExtActns,
  AdvOfficeSelectors, AdvOfficeComboBox;

type
  TActionClass = class of TAction;

  TCustomAdvToolBar = class(TAdvToolBar)
  public
    function AddButton(Instance: Cardinal; ActionClass: TActionClass; AStyle: TAdvButtonStyle; AResource: string; AHint: string): TAdvGlowButton;
  end;

  TAdvClipboardRibbonToolBar = class(TCustomAdvToolBar)
  private
    FCut,FCopy,FPaste: TAdvGlowButton;
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoPositionControls default false;
  end;

  TAdvFontRibbonToolBar = class(TCustomAdvToolBar)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TAdvAlignmentRibbonToolBar = class(TCustomAdvToolBar)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  StdActns, Graphics, PNGImage, Math, Windows;

const
  BTNSIZE = 24;

procedure MakeImageGrayscale(Image: TPNGImage; Amount: Byte = 255);

  procedure GrayscaleRGB(var R, G, B: Byte);
  var
    X: Byte;
  begin
    X := Round(R * 0.30 + G * 0.59 + B * 0.11);
    R := Round(R / 256 * (256 - Amount - 1)) + Round(X / 256 * (Amount + 1));
    G := Round(G / 256 * (256 - Amount - 1)) + Round(X / 256 * (Amount + 1));
    B := Round(B / 256 * (256 - Amount - 1)) + Round(X / 256 * (Amount + 1));
  end;

  procedure BrightnessRGB(var R, G, B: Byte);
  var
    ri,gi,bi: integer;
  begin
    ri := r;
    gi := g;
    bi := b;

    ri := Min(255, ri + 35);
    gi := Min(255, gi + 35);
    bi := Min(255, bi + 35);

    r := ri;
    g := gi;
    b := bi;
  end;
var
  X, Y, PalCount: Integer;
  Line: Pointer;
  PaletteHandle: HPalette;
  Palette: array[Byte] of TPaletteEntry;

begin
  //Don't do anything if the image is already a grayscaled one
  if not (Image.Header.ColorType in [COLOR_GRAYSCALE, COLOR_GRAYSCALEALPHA]) then
  begin
    if Image.Header.ColorType = COLOR_PALETTE then
    begin
      //Grayscale every palette entry
      PaletteHandle := Image.Palette;
      PalCount := GetPaletteEntries(PaletteHandle, 0, 256, Palette);
      for X := 0 to PalCount - 1
      do GrayscaleRGB(Palette[X].peRed, Palette[X].peGreen, Palette[X].peBlue);
      SetPaletteEntries(PaletteHandle, 0, PalCount, Palette);
      Image.Palette := PaletteHandle;
    end
    else
    begin
      //Grayscale every pixel
      for Y := 0 to Image.Height - 1 do
      begin
        Line := Image.Scanline[Y];
        for X := 0 to Image.Width - 1 do
          GrayscaleRGB(PRGBLine(Line)^[X].rgbtRed, PRGBLine(Line)^[X].rgbtGreen, PRGBLine(Line)^[X].rgbtBlue);
      end;
    end;
  end
  else
  begin
    if Image.Header.ColorType = COLOR_PALETTE then
    begin
    end
    else
    begin
      for Y := 0 to Image.Height - 1 do
      begin
        Line := Image.Scanline[Y];
        for X := 0 to Image.Width - 1 do
          BrightnessRGB(PRGBLine(Line)^[X].rgbtRed, PRGBLine(Line)^[X].rgbtGreen, PRGBLine(Line)^[X].rgbtBlue);
       end;
    end;
  end;
end;


{ TAdvClipboardRibbonToolBar }

constructor TAdvClipboardRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
begin
  inherited;
  Caption:= 'Clipboard';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  atb := AddButton(HInstance, TEditPaste, bsButton, 'TMSRIBBONPASTE', 'Paste from clipboard');
  atb.Tag := 1;
  atb.Caption := 'Paste';
  atb.ShowCaption := true;
  atb.DropDownButton := true;
  atb.DropDownPosition := dpBottom;
  atb.MinButtonSizeState := bsLarge;
  atb.MaxButtonSizeState := bsLarge;
  atb.Layout := blGlyphTop;
  atb.Width := 40;
  atb.Height := 64;

  FPaste := atb;

  atb := AddButton(HInstance, TEditCut, bsButton, 'TMSRIBBONCUT', 'Cut to clipboard');
  atb.Caption := 'Cut';
  atb.ShowCaption := true;
  atb.Tag := 2;
  atb.Width := 60;
  atb.Height := 24;
  atb.Left := 41;
  atb.Top := 2;

  FCut := atb;

  atb := AddButton(HInstance, TEditCopy, bsButton, 'TMSRIBBONCOPY', 'Copy to clipboard');
  atb.Caption := 'Copy';
  atb.ShowCaption := true;
  atb.Tag := 3;
  atb.Width := 60;
  atb.Height := 24;
  atb.Left := 41;
  atb.Top := 26;

  FCopy := atb;

  Width := 102;
  Height := 85;
end;

procedure TAdvClipboardRibbonToolBar.CreateWnd;
begin
  inherited;
  AutoPositionControls := false;
  ToolBarState := tsFixed;
  FCopy.Top := 24;
  FCopy.Left := 41;
end;

{ TAdvFontRibbonToolBar }

constructor TAdvFontRibbonToolBar.Create(AOwner: TComponent);
var
  atb: TAdvGlowButton;
  fs: TAdvOfficeFontSelector;
  fss: TAdvOfficeFontSizeSelector;
  cs: TAdvOfficeColorSelector;


begin
  inherited;

  Caption:= 'Font';
  CaptionAlignment := taCenter;
  CaptionPosition := cpBottom;
  ShowRightHandle := false;
  ShowCaption := true;

  ToolBarState := tsFixed;
  AutoPositionControls := false;
  AutoSize := false;

  fs := TAdvOfficeFontSelector.Create(Self);
  //fs.OnSelectFontName := SelectFontName;
  //fs.OnExit := ExitFontName;
  //actn := TAdvRichEditorFontName.Create(Self);
  //fs.Action := actn;
  AddToolBarControl(fs);
  fs.Left := 2;
  fs.Top := 2;


  fss := TAdvOfficeFontSizeSelector.Create(Self);
  //fss.OnSelectFontSize := SelectFontSize;
  //fss.OnExit := ExitFontSize;
  //actn := TAdvRichEditorFontSize.Create(Self);
  //fss.Action := actn;
  AddToolBarControl(fss);
  fss.Left := fs.Left + fs.Width;
  fss.Top := 2;


  atb := AddButton(HInstance, TRichEditBold, bsButton, 'TMSRIBBONBOLD', 'Bold');
  atb.Left := 2;
  atb.Top := 26;
  atb.Position := bpLeft;

  atb := AddButton(HInstance, TRichEditItalic, bsButton, 'TMSRIBBONITALIC', 'Italic');
  atb.Left := 26;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance, TRichEditUnderline, bsButton, 'TMSRIBBONUNDERLINE', 'Underline');
  atb.Left := 50;
  atb.Top := 26;
  atb.Position := bpMiddle;

  atb := AddButton(HInstance, TRichEditStrikeout, bsButton, 'TMSRIBBONSTRIKEOUT', 'Strikeout');
  atb.Left := 74;
  atb.Top := 26;
  atb.Position := bpRight;

  cs := TAdvOfficeColorSelector.Create(Self);
  cs.SelectedColor := clBlack;
  cs.ShowDisabled := false;
  //cs.OnSelectColor := SelectTextColor;
  cs.Tools.Items[0].BackGroundColor := Font.Color;
  //cs.Tag := integer(btTextColor);
  cs.OfficeHint.Title := 'Text color';
  cs.OfficeHint.Notes.Text := 'Set selection text color';
  cs.Top := 26;
  cs.Left := 102;
  cs.Width := 24;
  cs.Height := 24;
  cs.Position := bpLeft;

//  actn := TAdvRichEditorTextColor.Create(Self);
//  actn.ActionComponent := cs;
//  cs.Action := actn;

  AddToolBarControl(cs);

  cs := TAdvOfficeColorSelector.Create(Self);
  //cs.OnSelectColor := SelectColor;
  cs.SelectedColor := clWhite;
  cs.ShowDisabled := false;
  cs.Tools.Items[0].BackGroundColor := Color;


  //cs.Tag := integer(btBackgroundColor);
  cs.OfficeHint.Title := 'Background color';
  cs.OfficeHint.Notes.Text := 'Set selection background color';
  cs.Top := 26;
  cs.Left := 126;
  cs.Width := 24;
  cs.Height := 24;
  cs.Position := bpRight;
  AddToolBarControl(cs);

end;

{ TAdvAlignmentRibbonToolBar }

constructor TAdvAlignmentRibbonToolBar.Create(AOwner: TComponent);
begin
  inherited;

end;

{ TAdvRibbonToolBar }

function TCustomAdvToolBar.AddButton(Instance: Cardinal; ActionClass: TActionClass;
  AStyle: TAdvButtonStyle; AResource, AHint: string): TAdvGlowButton;
var
  atb: TAdvGlowButton;
  png: TPNGImage;
begin
  atb := TAdvGlowButton.Create(Self);
  atb.Width := BTNSIZE;
  atb.Height := BTNSIZE;
  atb.ShowDisabled := false;


  atb.Picture.LoadFromResourceName(Instance, AResource);
  if Assigned(actionclass) then
    atb.Action := ActionClass.Create(Self);
  atb.ShowCaption := false;
  atb.Hint := AHint;
  atb.Style := AStyle;
  AddToolBarControl(atb);

  png := TPNGImage.Create;
  png.LoadFromResourceName(Instance, AResource);
  MakeImageGrayscale(png,255);
  atb.DisabledPicture.Assign(png);
  png.Free;

  Result := atb;

end;

end.
