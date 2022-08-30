{***************************************************************************}
{ AdvRichEditorFormatButtonBar component                                    }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvRichEditorFormatButtonBar;

{$I TMSDEFS.INC}

interface

uses
  Classes, Extctrls, Graphics, AdvRichEditor, AdvRichEditorBase,
  ActnList, AdvOfficeSelectors, AdvOfficeComboBox, AdvGlowButton, SysUtils, Buttons,
  ExtActns, StdActns;

type
  TControlTag = (ctFont, ctSize, ctSpecialChars, ctListStyle);

  TAdvRichEditorFormatButtonBar = class(TCustomPanel)
  private
    FRichEditor: TAdvRichEditor;
    procedure SetRichEditor(const Value: TAdvRichEditor);
    function GetVersion: string;
    function GetVersionNr: Integer;
  protected
    function AddSpeedButton(AParent: TCustomPanel; AAction: TActionClass; APosition: Integer; AHint, AResource: string): TSpeedButton;
    procedure SetFontStyle(Sender: TObject);
    procedure SetFontSize(Sender: TObject);
    procedure SelectPicture(Sender: TObject);
    procedure SelectSpecialChar(Sender: TObject);
    procedure InsertHyperLink(Sender: TObject);
    procedure SelectBullet(Sender: TObject);
    procedure SetFontColor(Sender: TObject);
    procedure SetBackgroundHighlight(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
  published
    property Align;
    property Anchors;

    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;

    property Color;
    property Constraints;
    property Ctl3D;

    property DoubleBuffered;

    property Enabled;

    property Font;

    property Height;

    property Locked;

    property Padding;

    property RichEditor: TAdvRichEditor read FRichEditor write SetRichEditor;

    property ShowCaption;
    property ShowHint;
    {$IFDEF DELPHIXE5_LVL}
    property StyleElements;
    {$ENDIF}

    property TabOrder;

    property Version: string read GetVersion;
    property Visible;
  end;

implementation

{$R AdvRichEditorFormatButtonBar.res}

uses
  Dialogs, AdvRichEditorIO, StdCtrls, Forms, ExtDlgs, Windows;

const
  BTNSIZE = 24;

{ TAdvRichEditorFormatButtonBar }

function TAdvRichEditorFormatButtonBar.AddSpeedButton(AParent: TCustomPanel; AAction: TActionClass; APosition: Integer; AHint, AResource: string): TSpeedButton;
var
  btn: TSpeedButton;
begin
  btn := TSpeedButton.Create(AParent);
  if Assigned(AAction) then
    btn.Action := AAction.Create(Self);

  btn.Hint := AHint;
  btn.Width := BTNSIZE;
  btn.Height := BTNSIZE;
  btn.Left := 5 + ( APosition * BTNSIZE);
  btn.Parent := AParent;
  btn.ShowHint := true;

  btn.Glyph.LoadFromResourceName(HInstance,AResource);
  btn.NumGlyphs := 2;
  btn.Flat := true;

  Result := btn;
end;

constructor TAdvRichEditorFormatButtonBar.Create(AOwner: TComponent);
var
  I: Integer;
  btn: TSpeedButton;
  cbo: TComboBox;
begin
  inherited;

  I := 0;
  FRichEditor := nil;

  ShowCaption := false;
  Caption := EmptyStr;
  Width := 655;
  Height := 25;
  Text := EmptyStr;

  // font picker
  cbo := TComboBox.Create(Self);
  cbo.Left := 5;
  cbo.hint := 'Font Styles';
  cbo.Width := 4 * BTNSIZE;
  cbo.Top := 1;
  cbo.Tag := Integer(ctFont);
  cbo.OnSelect := SetFontStyle;
  cbo.Parent := Self;
  cbo.ShowHint := True;
  cbo.Style := csDropDownList;
  Inc(I, 4);

  // font size
  cbo := TComboBox.Create(Self);
  cbo.Left := 5 + (I * BTNSIZE);
  cbo.Hint := 'Font Size';
  cbo.Width := 2* BTNSIZE;
  cbo.Top := 1;
  cbo.Tag := Integer(ctSize);
  cbo.OnSelect := SetFontSize;
  cbo.Parent := Self;
  cbo.ShowHint := True;
  Inc(I,2);

  // Bold
  btn := AddSpeedButton(Self, TAdvRichEditorBold, I, 'Bold', 'TMSEDBBBOLD');
  btn.Parent := Self;
  Inc(I);

  // Italic
  btn := AddSpeedButton(Self, TAdvRichEditorItalic, I, 'Italic', 'TMSEDBBITALIC');
  btn.Parent := Self;
  Inc(I);

  // Underline
  btn := AddSpeedButton(Self, TAdvRichEditorUnderline, I, 'Underline', 'TMSEDBBUNDERLINE');
  btn.Parent := Self;
  Inc(I);

  // Strikethrough
  btn := AddSpeedButton(Self, TAdvRichEditorStrikeOut, I, 'Strikethrough', 'TMSEDBBSTRIKETHROUGH');
  btn.Parent := Self;
  Inc(I);

  // Subscript
  btn := AddSpeedButton(Self, TAdvRichEditorSubscript, I, 'Subscript', 'TMSEDBBSUBSCRIPT');
  btn.Parent := Self;
  Inc(I);

  // Superscript
  btn := AddSpeedButton(Self, TAdvRichEditorSuperscript, I, 'Superscript', 'TMSEDBBSUPERSCRIPT');
  btn.Parent := Self;
  Inc(I);

  // Align left
  btn := AddSpeedButton(Self, TAdvRichEditorAlignLeft, I, 'Align left', 'TMSEDBBALIGNLEFT');
  btn.Parent := Self;
  Inc(I);

  // Align center
  btn := AddSpeedButton(Self, TAdvRichEditorAlignCenter, I, 'Align center', 'TMSEDBBALIGNCENTER');
  btn.Parent := Self;
  Inc(I);

  // Align right
  btn := AddSpeedButton(Self, TAdvRichEditorAlignRight, I, 'Align right', 'TMSEDBBALIGNRIGHT');
  btn.Parent := Self;
  Inc(I);

  // color 1
  btn := AddSpeedButton(Self, nil, I, 'Font Color', 'TMSEDBBFONTCOLOR');
  btn.OnClick := SetFontColor;
  btn.Parent := Self;
  Inc(I);

  // color 2
  btn := AddSpeedButton(Self, nil, I, 'Text background color', 'TMSEDBBBACKGROUND');
  btn.OnClick := SetBackgroundHighlight;
  btn.Parent := Self;
  Inc(I);

  // picture
  btn := AddSpeedButton(Self, nil, I, 'Insert picture', 'TMSEDBBPICTURE');
  btn.Parent := Self;
  btn.OnClick := SelectPicture;
  Inc(I);

  // hyperlink
  btn := AddSpeedButton(Self, nil, I, 'Insert hyperlink', 'TMSEDBBLINK');
  btn.OnClick := InsertHyperlink;
  btn.Parent := Self;
  Inc(I);

  // special chars
  cbo := TComboBox.Create(Self);
  cbo.Hint := 'Special Chars';
  cbo.Left := 5 + (I * BTNSIZE);
  cbo.Width := 2 * BTNSIZE;
  cbo.Top := 1;
  cbo.Tag := Integer(ctSpecialChars);
  cbo.OnSelect := selectSpecialChar;
  cbo.Parent := Self;
  cbo.ShowHint := True;
  cbo.Style := csDropDownList;
  Inc(I,2);

  // bullets
  cbo := TComboBox.Create(Self);
  cbo.Left := 5 + (I * BTNSIZE);
  cbo.Width := 3 * BTNSIZE;
  cbo.Top := 1;
  cbo.Tag := Integer(ctListStyle);
  cbo.OnSelect := SelectBullet;
  cbo.Hint := 'Bullet Style';
  cbo.ShowHint := True;
  cbo.Style := csDropDownList;
  cbo.Parent := Self;
  Inc(I,3);

  // lists
  btn := AddSpeedButton(Self, TAdvRichEditorNumberedBulletType, I, 'Numbered lists', 'TMSEDBBNUMBERED');
  btn.Parent := Self;
  Inc(I);

  // Indent less
  btn := AddSpeedButton(Self, TAdvRichEditorUnIndent, I, 'Indent less', 'TMSEDBBINDENTLESS');
  btn.Parent := Self;
  Inc(I);

  // Indent more
  btn := AddSpeedButton(Self, TAdvRichEditorIndent, I, 'Indent more', 'TMSEDBBINDENTMORE');
  btn.Parent := Self;
end;

procedure TAdvRichEditorFormatButtonBar.CreateWnd;
var
  c: Integer;
  sl: TStringList;
begin
  inherited;
  for c := 0 to ControlCount - 1 do
  begin
    if Controls[c] is TComboBox then
    begin
      if (Controls[c] as TComboBox).Tag = Integer(ctFont) then
      begin
        (Controls[c] as TComboBox).Items.Assign(Screen.fonts);
        (Controls[c] as TComboBox).ItemIndex := 0;
      end;

      if (Controls[c] as TComboBox).Tag = Integer(ctSize) then
      begin
        sl := TStringList.Create;
        sl.Add('8');
        sl.Add('9');
        sl.Add('10');
        sl.Add('11');
        sl.Add('12');
        sl.Add('14');
        sl.Add('16');
        sl.Add('18');
        sl.Add('20');
        sl.Add('22');
        sl.Add('24');
        sl.Add('26');
        sl.Add('28');
        sl.Add('36');
        sl.Add('48');
        sl.Add('72');
        (Controls[c] as TComboBox).Items.Assign(sl);
        sl.Free;

        (Controls[c] as TComboBox).ItemIndex := 0;
      end;

      if (Controls[c] as TComboBox).Tag = Integer(ctSpecialChars) then
      begin
        sl := TStringList.Create;
        sl.Add('©');
        sl.Add('®');
        sl.Add('™');
        sl.Add('¼');
        sl.Add('½');
        sl.Add('¾');
        sl.Add('±');
        sl.Add('«');
        sl.Add('»');
        (Controls[c] as TComboBox).Items.Assign(sl);
        sl.Free;
        (Controls[c] as TComboBox).ItemIndex := 0;
      end;

      if (Controls[c] as TComboBox).Tag = Integer(ctListStyle) then
      begin
        sl := TStringList.Create;
        sl.Add('Circle');
        sl.Add('SQuare');
        sl.Add('Arrow');
        sl.Add('Checkmark');
        sl.Add('Star');
        (Controls[c] as TComboBox).Items.Assign(sl);
        sl.Free;
        (Controls[c] as TComboBox).ItemIndex := 0;
      end;
    end;
  end;
end;


procedure TAdvRichEditorFormatButtonBar.InsertHyperLink(Sender: TObject);
var
  url: string;
begin
  if Assigned(RichEditor) then
  begin
    url := '';
    InputQuery('Hyperlink','URL',url);
    RichEditor.SetSelectionHyperlink(url);
  end;
end;

procedure TAdvRichEditorFormatButtonBar.SetBackgroundHighlight(Sender: TObject);
var
  cd: TColorDialog;
begin
  cd := TColorDialog.Create(Self);
  if cd.Execute then
  begin
    if Assigned(RichEditor) then
    begin
      RichEditor.SetSelectionBkColor(cd.Color);
    end;
  end;
end;

procedure TAdvRichEditorFormatButtonBar.SetFontColor(Sender: TObject);
var
  cd: TColorDialog;
begin
  cd := TColorDialog.Create(Self);

  if cd.Execute then
    if Assigned(RichEditor) then
      RichEditor.SetSelectionColor(cd.Color);
end;

procedure TAdvRichEditorFormatButtonBar.SetFontSize(Sender: TObject);
var
  s: Integer;
begin
  s := StrToInt(TComboBox(Sender).Items[TComboBox(Sender).ItemIndex]);
  FRichEditor.SetSelectionFontSize(s);
end;

procedure TAdvRichEditorFormatButtonBar.SetFontStyle(Sender: TObject);
var
  font: string;
begin
  font := TComboBox(Sender).Items[TComboBox(Sender).ItemIndex];
  FRichEditor.SetSelectionFontName(font);
end;

procedure TAdvRichEditorFormatButtonBar.SelectBullet(Sender: TObject);
begin
  if Assigned(RichEditor) then
  begin
    RichEditor.SetSelectionBullets(TBulletType(TComboBox(Sender).ItemIndex));
    RichEditor.SetFocus;
  end;
end;

procedure TAdvRichEditorFormatButtonBar.SelectPicture(Sender: TObject);
var
  pd: TOpenPictureDialog;
begin
  if Assigned(RichEditor) then
  begin
    pd := TOpenPictureDialog.Create(Self);
    try
      if pd.Execute then
      begin
        RichEditor.InsertImage(pd.FileName);
        RichEditor.SetFocus;
      end;
    finally
      pd.Free;
    end;
  end;
end;

procedure TAdvRichEditorFormatButtonBar.SelectSpecialChar(Sender: TObject);
var
  ch: Char;
begin
  ch := TComboBox(Sender).Items[TComboBox(Sender).ItemIndex][1];
  FRichEditor.InsertChar(ch);
end;

procedure TAdvRichEditorFormatButtonBar.SetRichEditor(const Value: TAdvRichEditor);
begin
  FRichEditor := Value;
end;

function TAdvRichEditorFormatButtonBar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvRichEditorFormatButtonBar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

end.
