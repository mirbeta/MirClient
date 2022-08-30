{===============================================================================
  RzRadioGroupEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzStringListProperty
    This unit file contains a replacement string list property editor and its 
    associated dialog box. After installing this unit into the Delphi IDE, this
    editor will be used to edit all string list properties.


  Modification History
  ------------------------------------------------------------------------------
  6.2.2  (09 Apr 2016)
    * Fixed issue with restoring the String List Editor to a position that is
      longer valid for the current screen resolution.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed problem where the Copy toolbar button was not enabled after clicking
      the Select All button.
    * Updated the Open/Save dialogs to use new Vista/Windows 7 versions if
      available.
    * Cleaned up the appearance of the string list editor.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Fixed problem with Indent and Unindent operations in the Raize String List
      Editor.
    * The Tab key can now be used to tab to the OK and Cancel buttons. Press
      Ctrl+Tab to insert a tab character (#9) into the edit area.
    * Added a new Select All button to the Raize String List Editor toolbar.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed positioning of Raize String List Editor under Delphi 2005 and
      Borland Developer Studio 2006.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Updated form to use custom framing editing controls and HotTrack style
      buttons, radio buttons, and check boxes.
===============================================================================}

{$I RzComps.inc}

unit RzStringListEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  RzPanel,
  StdCtrls,
  Buttons,
  IniFiles,
  DesignIntf,
  DesignEditors,
  Registry,
  Menus,
  RzPrgres,
  RzSpnEdt,
  RzBorder,
  RzStatus,
  Mask,
  RzEdit,
  RzButton, 
  ImgList;

type
  TRzStringListProperty = class( TPropertyEditor )
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;


  TRzStringListEditDlg = class( TForm )
    mnuEdit: TPopupMenu;
    mnuUndo: TMenuItem;
    mnuCut: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    mnuSep1: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuSep2: TMenuItem;
    mnuIndent: TMenuItem;
    dlgFont: TFontDialog;
    mnuUnindent: TMenuItem;
    mnuPrint: TMenuItem;
    mnuSep3: TMenuItem;
    dlgPrint: TPrintDialog;
    pnlButtons: TRzPanel;
    RzPanel2: TRzPanel;
    btnOk: TRzButton;
    btnCancel: TRzButton;
    pnlWorkSpace: TRzPanel;
    edtStrings: TRzMemo;
    btnCodeEditor: TRzButton;
    mnuSelectAll: TMenuItem;
    ImageList1: TImageList;
    RzToolbar1: TRzToolbar;
    btnUnindent: TRzToolButton;
    btnIndent: TRzToolButton;
    RzSpacer1: TRzSpacer;
    btnOpen: TRzToolButton;
    btnSave: TRzToolButton;
    RzSpacer2: TRzSpacer;
    btnPrint: TRzToolButton;
    RzSpacer3: TRzSpacer;
    btnCut: TRzToolButton;
    btnCopy: TRzToolButton;
    btnPaste: TRzToolButton;
    btnUndo: TRzToolButton;
    RzSpacer4: TRzSpacer;
    btnFont: TRzToolButton;
    RzSpacer5: TRzSpacer;
    btnTabSize: TRzToolButton;
    RzSpacer6: TRzSpacer;
    spnTabSize: TRzSpinEdit;
    RzSpacer7: TRzSpacer;
    btnSetTabSize: TRzToolButton;
    btnCancelTabSize: TRzToolButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    btnSelectAll: TRzToolButton;
    RzSpacer8: TRzSpacer;
    RzStatusBar1: TRzStatusBar;
    stsLine: TRzFieldStatus;
    stsColumn: TRzFieldStatus;
    stsLineCount: TRzStatusPane;
    pbrPrint: TRzProgressStatus;
    procedure mnuSelectAllClick(Sender: TObject);
    procedure FormCreate( Sender: TObject );
    procedure FormDestroy( Sender: TObject );
    procedure btnFontClick( Sender: TObject );
    procedure btnUndoClick( Sender: TObject );
    procedure btnCutClick( Sender: TObject );
    procedure btnCopyClick( Sender: TObject );
    procedure btnPasteClick( Sender: TObject );
    procedure btnOpenClick( Sender: TObject );
    procedure btnSaveClick( Sender: TObject );
    procedure btnIndentClick( Sender: TObject );
    procedure edtStringsChange( Sender: TObject );
    procedure edtStringsKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure edtStringsKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure edtStringsClick( Sender: TObject);
    procedure edtStringsMouseUp( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnTabSizeClick(Sender: TObject);
    procedure btnUnindentClick(Sender: TObject);
    procedure btnSetTabSizeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnCodeEditorClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSelectAllClick(Sender: TObject);
  private
    SingleLine: string;
    MultipleLines: string;
    DelphiIni: TRegIniFile;
    FTabSize: Integer;
    FCurLine: Integer;
    FCurCol: Integer;
    FPropName: string;
    FModified: Boolean;
    function EndOfLine( LineNum: Integer ): Integer;
    procedure IndentLine( LineNum: Integer );
    function UnindentLine( LineNum: Integer ): Boolean;
    procedure IndentLines( Indent: Boolean );
    procedure SetTabSize;
    procedure EnableButtons( Enable: Boolean );
    procedure WMGetMinMaxInfo( var Msg: TWMGetMinMaxInfo ); message wm_GetMinMaxInfo;
  public
    property PropName: string
      read FPropName
      write FPropName;

    procedure UpdateLineColStatus;
    procedure UpdateClipboardStatus;
    procedure UpdateButtonStatus;
  end;


implementation

{$R *.dfm}


uses
  SysUtils,
  LibHelp,
  ClipBrd,
  Printers,
  ToolsAPI,
  StFilSys,
  TypInfo,
  RzStringModule,
  RzCommon;

const
  Section = 'String List Editor';

  fsBoldMask      = 8;              { Constants used to determine font style }
  fsItalicMask    = 4;
  fsUnderlineMask = 2;
  fsStrikeOutMask = 1;
  fsNormal        = 0;
  
  //Temp fix for opening the strings in the editor.
  DotSep = '.';

{===================================}
{== TRzStringListProperty Methods ==}
{===================================}

function TRzStringListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [ paDialog ] - [ paSubProperties ];
end;


function TRzStringListProperty.GetValue: string;
begin
  { The GetPropType method is used to retrieve information pertaining to the   }
  { property type being edited.  In this case, the Name of the property class  }
  { is displayed in the value column of the Object Inspector.                  }

  Result := Format( '(%s)', [ GetPropType^.Name ] );
end;


procedure TRzStringListProperty.Edit;
var
  Ident: string;
  Component: TComponent;
  Module: IOTAModule;
  Editor: IOTAEditor;
  ModuleServices: IOTAModuleServices;
  Stream: TStringStream;
  Age: TDateTime;
  Dialog: TRzStringListEditDlg;
begin
  Component := TComponent( GetComponent( 0 ) );

  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if ( TObject( Component ) is TComponent ) and
     ( Component.Owner = Self.Designer.GetRoot ) and
     ( Self.Designer.GetRoot.Name <> '' ) then
  begin
    Ident := Self.Designer.GetDesignerExtension + DotSep + Self.Designer.GetRoot.Name + DotSep +
             Component.Name + DotSep + GetName;

    Module := ModuleServices.FindModule( Ident );
  end
  else
    Module := nil;

  if ( Module <> nil ) and ( Module.GetModuleFileCount > 0 ) then
    Module.GetModuleFileEditor( 0 ).Show
  else
  begin
    Dialog := TRzStringListEditDlg.Create( Application );
    try
      if Ident <> '' then
        Dialog.FPropName := Ident
      else if TObject( Component ) is TComponent then
        Dialog.FPropName := Component.Name + GetName
      else
        Dialog.FPropName := GetName;
      Dialog.Caption :=  Dialog.FPropName + ' - ' + Dialog.Caption;

      { Copy string list of property into the memo field of the dialog }
      Dialog.edtStrings.Lines := TStrings( GetOrdValue );
      Dialog.UpdateLineColStatus;      { Update initial cursor position status }
      Dialog.FModified := False;

      Dialog.btnCodeEditor.Enabled := Ident <> '';

      case Dialog.ShowModal of
        mrOK:
          SetOrdValue( Longint( Dialog.edtStrings.Lines ) );

        mrYes:
        begin
          Stream := TStringStream.Create( AnsiToUTF8( Dialog.edtStrings.Lines.Text ) );

          Stream.Position := 0;
          Age := Now;
          Module := ModuleServices.CreateModule( TRzStringsModuleCreator.Create( Ident, Stream, Age ) );
          if Module <> nil then
          begin
            with StringsFileSystem.GetTStringsProperty( Ident, Component, GetName ) do
              DiskAge := DateTimeToFileDate( Age );
            Editor := Module.GetModuleFileEditor( 0 );
            if Dialog.FModified then
              Editor.MarkModified;
            Editor.Show;
          end;
        end;
      end; { case }
    finally
      Dialog.Free;
    end;
  end;
end;


resourcestring
  sRzEditorLine = 'Line';
  sRzEditorLines = 'Lines';

{=================================}
{== TRzStrListEditorDlg Methods ==}
{=================================}

procedure TRzStringListEditDlg.FormCreate(Sender: TObject);
var
  StyleBits: Byte;
  L, T: Integer;
begin
  PopupMode := pmAuto;
  Position := poDesigned;

  Icon.Handle := LoadIcon( HInstance, 'RZDESIGNEDITORS_EDIT_ITEMS_ICON' );


  { Load String Resources for Line/Lines Label }
  SingleLine := sRzEditorLine;
  MultipleLines := sRzEditorLines;

  DelphiIni := TRegIniFile.Create( 'Software\Raize' );

  with edtStrings.Font do
  begin
    Name := DelphiIni.ReadString( Section, 'FontName', 'Tahoma' );
    Size := DelphiIni.ReadInteger( Section, 'FontSize', 8 );
    Color := DelphiIni.ReadInteger( Section, 'FontColor', clWindowText );
    StyleBits := DelphiIni.ReadInteger( Section, 'FontStyle', fsNormal );
    Style := [];
    if StyleBits and fsBoldMask = fsBoldMask then
      Style := Style + [ fsBold ];
    if StyleBits and fsItalicMask = fsItalicMask then
      Style := Style + [ fsItalic ];
    if StyleBits and fsUnderlineMask = fsUnderlineMask then
      Style := Style + [ fsUnderline ];
    if StyleBits and fsStrikeOutMask = fsStrikeOutMask then
      Style := Style + [ fsStrikeOut ];
  end;
  FTabSize := DelphiIni.ReadInteger( Section, 'TabSize', 8 );
  Width := DelphiIni.ReadInteger( Section, 'Width', 420 );
  Height := DelphiIni.ReadInteger( Section, 'Height', 320 );

  L := DelphiIni.ReadInteger( Section, 'Left', ( Screen.Width - Width ) div 2 );
  if ( L - Width ) < 0 then
    L := 0
  else if L > ( Screen.Width - Width ) then
    L := Screen.Width - Width;
  Left := L;

  T := DelphiIni.ReadInteger( Section, 'Top', ( Screen.Height - Height ) div 2 );
  if ( T - Height ) < 0 then
    T := 0
  else if T > ( Screen.Height - Height ) then
    T := Screen.Height - Height;
  Top := T;

  UpdateClipboardStatus;

  spnTabSize.FlatButtons := True;

  btnCodeEditor.Visible := True;
end; {= TRzStrListEditorDlg.FormCreate =}


procedure TRzStringListEditDlg.FormDestroy(Sender: TObject);
begin
  DelphiIni.Free;
end;


procedure TRzStringListEditDlg.FormClose(Sender: TObject; var Action: TCloseAction);
var
  StyleBits: Byte;
begin
  with edtStrings.Font do
  begin
    DelphiIni.WriteString( Section, 'FontName', Name );
    DelphiIni.WriteInteger( Section, 'FontSize', Size );
    DelphiIni.WriteInteger( Section, 'FontColor', Color );

    StyleBits := 0;
    if fsBold in Style then
      StyleBits := fsBoldMask;
    if fsItalic in Style then
      StyleBits := StyleBits + fsItalicMask;
    if fsUnderline in Style then
      StyleBits := StyleBits + fsUnderlineMask;
    if fsStrikeOut in Style then
      StyleBits := StyleBits + fsStrikeOutMask;
    DelphiIni.WriteInteger( Section, 'FontStyle', StyleBits );
  end;
  DelphiIni.WriteInteger( Section, 'TabSize', FTabSize );
  DelphiIni.WriteInteger( Section, 'Left', Left );
  DelphiIni.WriteInteger( Section, 'Top', Top );
  DelphiIni.WriteInteger( Section, 'Width', Width );
  DelphiIni.WriteInteger( Section, 'Height', Height );
end;


procedure TRzStringListEditDlg.btnSelectAllClick(Sender: TObject);
begin
  edtStrings.SelectAll;
  UpdateClipboardStatus;
end;


procedure TRzStringListEditDlg.btnFontClick(Sender: TObject);
begin
  dlgFont.Font := edtStrings.Font;
  if dlgFont.Execute then
  begin
    edtStrings.Font := dlgFont.Font;           { Assign new font to Memo field }
  end;
end;


procedure TRzStringListEditDlg.btnUndoClick(Sender: TObject);
begin
  edtStrings.Perform( wm_Undo, 0, 0 );
end;


procedure TRzStringListEditDlg.btnCutClick(Sender: TObject);
begin
  edtStrings.CutToClipboard;
  UpdateClipboardStatus;
end;


procedure TRzStringListEditDlg.btnCopyClick(Sender: TObject);
begin
  edtStrings.CopyToClipboard;
  UpdateClipboardStatus;
end;


procedure TRzStringListEditDlg.btnPasteClick(Sender: TObject);
begin
  edtStrings.PasteFromClipboard;
end;


procedure TRzStringListEditDlg.mnuSelectAllClick(Sender: TObject);
begin
  edtStrings.SelectAll;
end;


procedure TRzStringListEditDlg.btnOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtStrings.Lines.LoadFromFile( dlgOpen.FileName );
end;


procedure TRzStringListEditDlg.btnSaveClick(Sender: TObject);
{$IFDEF UNICODE}
var
  UseANSI: Boolean;
{$ENDIF}
begin
  if dlgSave.Execute then
  begin
    {$IFDEF UNICODE}
    UseANSI := edtStrings.Lines.Text = UnicodeString( AnsiString( edtStrings.Lines.Text ) );

    if UseANSI then
      edtStrings.Lines.SaveToFile( dlgSave.FileName, TEncoding.Default )
    else
      edtStrings.Lines.SaveToFile( dlgSave.FileName, TEncoding.UTF8 );

    {$ELSE}
    edtStrings.Lines.SaveToFile( dlgSave.FileName );
    {$ENDIF}
  end;
end;


procedure TRzStringListEditDlg.btnPrintClick(Sender: TObject);
const
  LM = '        ';
var
  I: Integer;
  PrintText: TextFile;
  Header: string;
begin
  if dlgPrint.Execute then
  begin
    PbrPrint.TotalParts := edtStrings.Lines.Count;

    AssignPrn( PrintText );
    try
      Rewrite( PrintText );
      try
        Printer.Canvas.Font.Name := 'Arial';
        Printer.Canvas.Font.Style := [ fsBold ];
        Printer.Canvas.Font.Size := 12;

        Header := 'Contents of the ';
        if FPropName <> '' then
          Header := Header + FPropName + ' ';
        Header := Header + 'String List';

        Writeln( PrintText );
        Writeln( PrintText );
        Writeln( PrintText, LM, Header );
        Writeln( PrintText );
        Header := 'Printed on ' + FormatDateTime( 'ddddd "at" t', Now );
        Writeln( PrintText, LM, Header );

        Printer.Canvas.Font.Name := 'Courier New';
        Printer.Canvas.Font.Style := [];
        Printer.Canvas.Font.Size := 10;

        for I := 0 to edtStrings.Lines.Count - 1 do
        begin
          Writeln( PrintText, LM, edtStrings.Lines[ I ] );   { Print each line }
          PbrPrint.IncPartsByOne;          { Update percentage of Progress Bar }
        end;
      finally
        CloseFile( PrintText );        { Ensures that Printer File gets closed }
      end;
    finally
      PbrPrint.Percent := 0;
    end;
  end;
end;  {= TRzStrListEditorDlg.btnPrintClick =}


function TRzStringListEditDlg.EndOfLine( LineNum: Integer ): Integer;
var
  L: Longint;
  P: Integer;
begin
  with edtStrings do
  begin
    L := Perform( em_LineIndex, LineNum + 1, 0 ) - 2;
    if Integer( L ) < 0 then
    begin
      L := Perform( em_LineIndex, LineNum, 0 );
      P := Perform( em_LineLength, L, 0 );
      Result := L + P;
    end
    else
      Result := L;
  end;
end;


procedure TRzStringListEditDlg.IndentLine( LineNum: Integer );
begin
  // Move to Beginning of line and insert tab
  edtStrings.SelStart := edtStrings.Perform( em_LineIndex, LineNum, 0 );
  edtStrings.Perform( wm_Char, vk_tab, 0 );
  // Move cursor to end of line
  edtStrings.SelStart := EndOfLine( LineNum );
  UpdateLineColStatus;
end;


function TRzStringListEditDlg.UnindentLine( LineNum: Integer ): Boolean;
var
  L: string;
begin
  L := edtStrings.Lines[ LineNum ];
  if ( Length( L ) > 0 ) and ( L[ 1 ] = #9 ) then
  begin
    // Move to Beginning of line and remove first character
    edtStrings.SelStart := edtStrings.Perform( em_LineIndex, LineNum, 0 );
    edtStrings.Perform( wm_KeyDown, vk_Delete, 0 );
    edtStrings.Perform( wm_KeyUp, vk_Delete, 0 );

    // Move cursor to end of line
    edtStrings.SelStart := EndOfLine( LineNum );
    Result := True;
    UpdateLineColStatus;
  end
  else
    Result := False;
end; {= TRzStrListEditorDlg.UnindentLine =}


procedure TRzStringListEditDlg.IndentLines( Indent: Boolean );
var
  I, StartLine, StopLine: Integer;
  OldSelStart, OldSelLength: Integer;
  LineCount, P: Integer;
begin
  with edtStrings do
  begin
    StartLine := Perform( em_LineFromChar, SelStart, 0 );
    StopLine := Perform( em_LineFromChar, SelStart + SelLength, 0 );

    SelStart := Perform( em_LineIndex, StartLine, 0 );
    P := EndOfLine( StopLine );
    SelLength := P - SelStart;

    OldSelStart := SelStart;
    OldSelLength := SelLength;

    LineCount := 0;
    for I := StartLine to StopLine do   { For each line in the selection block }
    begin
      if Indent then
        IndentLine( I )
      else
      begin
        if UnindentLine( I ) then
          Inc( LineCount );
      end;
    end;

    SelStart := OldSelStart;                     { Re-establish text selection }
    if Indent then
      SelLength := OldSelLength + StopLine - StartLine
    else
      SelLength := OldSelLength - LineCount;

  end;
end;


{= TRzStrListEditorDlg.IndentLines =}


procedure TRzStringListEditDlg.btnUnindentClick(Sender: TObject);
begin
  if edtStrings.SelLength <> 0 then
    IndentLines( False )
  else
    UnindentLine( FCurLine );
  UpdateClipboardStatus;
end;


procedure TRzStringListEditDlg.btnIndentClick(Sender: TObject);
begin
  with edtStrings do
  begin
    if SelLength <> 0 then
      IndentLines( True )
    else
      IndentLine( FCurLine );
  end;
  UpdateClipboardStatus;
end;


procedure TRzStringListEditDlg.edtStringsChange(Sender: TObject);
var
  Count: Integer;
  LineText: string;
begin
  FModified := True;
  Count := edtStrings.Lines.Count;
  if Count = 1 then
    LineText := SingleLine
  else
    LineText := MultipleLines;
  stsLineCount.Caption := Format( '%d %s', [ Count, LineText ] );

  UpdateButtonStatus;
  UpdateLineColStatus;
end;


procedure TRzStringListEditDlg.edtStringsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  UpdateLineColStatus;
  { If user presses Esc key while in Memo, the Dialog is cancelled }
  if Key = vk_Escape then
    btnCancel.Click;
end;


procedure TRzStringListEditDlg.edtStringsKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  UpdateLineColStatus;
end;


procedure TRzStringListEditDlg.edtStringsClick(Sender: TObject);
begin
  UpdateLineColStatus;
end;


procedure TRzStringListEditDlg.UpdateLineColStatus;
begin
  { Get current line from cursor position }
  FCurLine := edtStrings.Perform( em_LineFromChar, edtStrings.SelStart, 0 );
  { Get current column from cursor position }
  FCurCol := edtStrings.SelStart - edtStrings.Perform( em_LineIndex, FCurLine, 0 );

  stsLine.Caption := IntToStr( FCurLine + 1 );
  stsColumn.Caption := IntToStr( FCurCol + 1 );
  UpdateClipboardStatus;
end;


procedure TRzStringListEditDlg.UpdateClipboardStatus;
var
  HasText: Boolean;
  HasSelection: Boolean;
begin
  HasSelection := edtStrings.SelLength <> 0;
  btnCut.Enabled := HasSelection;           { Cut and Copy are only enabled if }
  MnuCut.Enabled := HasSelection;            { the user has selected some text }
  btnCopy.Enabled := HasSelection;
  MnuCopy.Enabled := HasSelection;

  HasText := Clipboard.HasFormat( cf_Text );
  btnPaste.Enabled := HasText;                  { Paste is only enabled if the }
  MnuPaste.Enabled := HasText;                       { Clipboard contents Text }
end;


procedure TRzStringListEditDlg.UpdateButtonStatus;
var
  Enable: Boolean;
begin
  Enable := edtStrings.Lines.Count <> 0;
  { No point in Unindenting, Saving, or Printing when there is no text in memo }
  btnUnindent.Enabled := Enable;
  btnSave.Enabled := Enable;
  btnPrint.Enabled := Enable;
end;


procedure TRzStringListEditDlg.edtStringsMouseUp( Sender: TObject;
                                                 Button: TMouseButton;
                                                 Shift: TShiftState;
                                                 X, Y: Integer);
begin
  UpdateClipboardStatus;
end;


procedure TRzStringListEditDlg.btnTabSizeClick(Sender: TObject);
begin
  if btnTabSize.Down then
  begin
    { When the TabSize button is pressed, all of the controls in the dialog    }
    { are disabled, in effect creating a modal state in which the tab size of  }
    { the memo field can be updated.                                           }

    spnTabSize.Visible := True;
    btnSetTabSize.Visible := True;
    btnCancelTabSize.Visible := True;
    EnableButtons( False );                              { Disable All Buttons }

    spnTabSize.Value := FTabSize;
    spnTabSize.SetFocus;
  end
  else
    btnSetTabSizeClick( btnCancelTabSize );
end; {= TRzStrListEditorDlg.btnTabSizeClick =}


procedure TRzStringListEditDlg.SetTabSize;
var
  TabStop: Integer;
begin
  if FTabSize < 0 then
    FTabSize := -FTabSize;
  TabStop := FTabSize * 4;              { Roughly 4 Dialog units per character }
  edtStrings.Perform( em_SetTabStops, 1, LParam( @TabStop ) );
  edtStrings.Invalidate;
end;


procedure TRzStringListEditDlg.btnSetTabSizeClick(Sender: TObject);
begin
  if Sender = btnSetTabSize then
  begin
    try
      FTabSize := spnTabSize.IntValue;
    except
    end;
    SetTabSize;
  end;

  spnTabSize.Visible := False;
  btnSetTabSize.Visible := False;
  btnCancelTabSize.Visible := False;
  btnTabSize.Down := False;
  EnableButtons( True );
  edtStrings.SetFocus;
end;


procedure TRzStringListEditDlg.EnableButtons( Enable: Boolean );
var
  SysMenu: HMenu;
begin
  btnUnindent.Enabled := Enable;
  btnIndent.Enabled := Enable;
  btnOpen.Enabled := Enable;
  btnSave.Enabled := Enable;
  btnPrint.Enabled := Enable;
  btnUndo.Enabled := Enable;
  btnFont.Enabled := Enable;
  btnOK.Enabled := Enable;
  btnCancel.Enabled := Enable;
  btnCodeEditor.Enabled := Enable;

  edtStrings.Enabled := Enable;

  btnCut.Enabled := Enable;
  btnCopy.Enabled := Enable;
  btnPaste.Enabled := Enable;
  if Enable then
    UpdateClipboardStatus;
  if Enable then
    UpdateButtonStatus;

  { Disable the Close menu item, so dialog cannot be closed }
  SysMenu := GetSystemMenu( Handle, False );
  if Enable then
    EnableMenuItem( SysMenu, sc_Close, mf_ByCommand or mf_Enabled )
  else
    EnableMenuItem( SysMenu, sc_Close, mf_ByCommand or mf_Disabled or mf_Grayed );
end; {= TRzStringListEditDlg.EnableButtons =}


procedure TRzStringListEditDlg.FormShow(Sender: TObject);
begin
  SetTabSize;
end;


procedure TRzStringListEditDlg.WMGetMinMaxInfo( var Msg: TWMGetMinMaxInfo );
begin
  Msg.MinMaxInfo^.ptMinTrackSize := Point( 410, 220 );
end;


procedure TRzStringListEditDlg.btnCodeEditorClick(Sender: TObject);
begin
  ModalResult := mrYes;
end;


end.

