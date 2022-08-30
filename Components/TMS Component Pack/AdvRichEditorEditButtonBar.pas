{***************************************************************************}
{ AdvRichEditorEditButtonBar component                                      }
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

unit AdvRichEditorEditButtonBar;

{$I TMSDEFS.INC}

interface

uses
  Classes, Extctrls, Graphics, AdvRichEditor, AdvRichEditorBase,
  ActnList, AdvOfficeSelectors, AdvOfficeComboBox, AdvGlowButton, SysUtils,
  Buttons, ExtActns, StdActns;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.10  : New : Added support to open RTF files directly from TAdvRichEditorEditButtonBar

type
  TAdvRichEditorEditButtonBar = class(TCustomPanel)
  private
    FRichEditor: TAdvRichEditor;
    FRecentFileName: string;
    procedure OpenFile(Sender: TObject);
    procedure SetRichEditor(const Value: TAdvRichEditor);
    function GetVersion: string;
    function GetVersionNr: Integer;
  protected
    function AddSpeedButton(AParent: TCustomPanel; AAction: TActionClass; APosition: Integer; AHint, AResource: string): TSpeedButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveToHTML(AFileName: string);
    procedure SaveToRTF(AFileName: string);
    procedure SaveFile(Sender: TObject);
    property RecentFileName: string read FRecentFileName;
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

{$R AdvRichEditorEditButtonBar.RES}

uses
  Dialogs, AdvRichEditorIO, Windows;

const
  BTNSIZE = 24;

{ TAdvRichEditorFormatButtonBar }

function TAdvRichEditorEditButtonBar.AddSpeedButton(AParent: TCustomPanel; AAction: TActionClass; APosition: Integer; AHint, AResource: string): TSpeedButton;
var
  btn: TSpeedButton;
begin
  btn := TSpeedButton.Create(AParent);

  if Assigned(AAction) then
    btn.Action := AAction.Create(Self);

  btn.Hint := AHint;
  btn.Width := BTNSIZE;
  btn.Height := BTNSIZE;
  btn.Left := 5 + (APosition * BTNSIZE);
  btn.Parent := AParent;
  btn.ShowHint := true;

  btn.Glyph.LoadFromResourceName(HInstance, AResource);
  btn.NumGlyphs := 2;
  btn.Flat := true;

  Result := btn;
end;

constructor TAdvRichEditorEditButtonBar.Create(AOwner: TComponent);
var
  I: Integer;
  btn: TSpeedButton;
begin
  inherited;
  I := 0;

  ShowCaption := false;
  Caption := EmptyStr;
  Width := 180;
  Height := 25;
  Text := EmptyStr;

  // open file
  btn := AddSpeedButton(Self, nil, I, 'Open a file', 'TMSEDBBOPEN');
  btn.OnClick := OpenFile;
  btn.Parent := Self;
  Inc(I);

  // save file
  btn := AddSpeedButton(Self, nil, I, 'Save', 'TMSEDBBSAVE');
  btn.OnClick := SaveFile;
  btn.Parent := Self;
  Inc(I);

  // cut
  btn := AddSpeedButton(Self, TAdvRichEditorCut, I, 'Cut the selected text', 'TMSEDBBCUT');
  btn.Parent := Self;
  Inc(I);

  // paste
  btn := AddSpeedButton(Self, TAdvRichEditorPaste, I, 'Paste text from clipboard', 'TMSEDBBCOPY');
  btn.Parent := Self;
  Inc(I);

  // copy
  btn := AddSpeedButton(Self, TAdvRichEditorCopy, I, 'Copy selected text', 'TMSEDBBPASTE');
  btn.Parent := Self;
  Inc(I);

  // Undo
  btn := AddSpeedButton(Self, TAdvRichEditorUndo, I, 'Undo', 'TMSEDBBUNDO');
  btn.Parent := Self;
  Inc(I);

  // Redo
  btn := AddSpeedButton(Self, TAdvRichEditorRedo, I, 'Redo', 'TMSEDBBREDO');
  btn.Parent := Self;
end;

procedure TAdvRichEditorEditButtonBar.OpenFile(Sender: TObject);
var
  od: TOpenDialog;
  fe: string;
  RTFIO: TAdvRichEditorRTFIO;

begin
  od := TOpenDialog.Create(Self);
  od.Filter := 'Text files|*.txt|RTF files|*.rtf|RTE files|*.rte|All files|*.*';
  try
    if od.Execute then
      if Assigned(FRichEditor) then
      begin
        FRecentFileName := od.FileName;
        fe := Uppercase(ExtractFileExt(od.FileName));

        if fe = '.TXT' then
          FRichEditor.LoadFromTextFile(od.FileName)
        else
        if fe = '.RTF' then
        begin
          RTFIO := TAdvRichEditorRTFIO.Create(Self);
          try
            RTFIO.RichEditor := FRichEditor;
            RTFIO.Load(od.FileName);
          finally
            RTFIO.Free;
          end;
        end
        else
          FRichEditor.LoadFromFile(od.FileName);
      end;
  finally
    od.Free;
  end;
end;

procedure TAdvRichEditorEditButtonBar.SaveFile(Sender: TObject);
var
  sd: TSaveDialog;
  fe,fn: string;
begin
  sd := TSaveDIalog.Create(Self);
  sd.Filter := 'Text files|*.txt|RTE files|*.rte|HTML files|*.htm|Rich text|*.rtf';
  try
    if sd.Execute then
    begin
      fn := sd.FileName;
      fe := Uppercase(ExtractFileExt(sd.FileName));

      if (fe = '') then
      begin
        if sd.FilterIndex = 1 then
          fn := fn + '.txt';
        if sd.FilterIndex = 2 then
          fn := fn + '.rte';
        if sd.FilterIndex = 3 then
          fn := fn + '.htm';
        if sd.FilterIndex = 4 then
          fn := fn + '.rtf';
      end;

      case sd.FilterIndex of
      1: FRichEditor.SaveToText(fn);
      2: FRichEditor.SaveToFile(fn);
      3: SaveToHTML(fn);
      4: SaveToRTF(fn);
      end;
    end;
  finally
    sd.Free;
  end;
end;

procedure TAdvRichEditorEditButtonBar.SaveToHTML(AFileName: string);
var
  html: TAdvRichEditorHTMLIO;
begin
  html := TAdvRichEditorHTMLIO.Create(Self);
  try
    html.RichEditor := FRichEditor;
    html.Save(AFileName,ExtractFilePath(AFileName));
  finally
    html.Free;
  end;
end;

procedure TAdvRichEditorEditButtonBar.SaveToRTF(AFileName: string);
var
  rtf: TAdvRichEditorRTFIO;
begin
  rtf := TAdvRichEditorRTFIO.Create(Self);
  try
    rtf.RichEditor := FRichEditor;
    rtf.Save(AFileName);
  finally
    rtf.Free;
  end;
end;

procedure TAdvRichEditorEditButtonBar.SetRichEditor(
  const Value: TAdvRichEditor);
begin
  FRichEditor := Value;
end;   

function TAdvRichEditorEditButtonBar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;    

function TAdvRichEditorEditButtonBar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

end.
