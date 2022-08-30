{===============================================================================
  RzSelDir Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzSelDirDialog
    Dialog-based component to allow user to select a directory.


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Deprecated.  This control has been replaced by the new Shell Dialog base
      controls.
    * Fixed problem where setting Directory property and setting FrameVisible to
      True would cause an access violation when the dialog was displayed.
===============================================================================}

{$I RzComps.inc}

unit RzSelDir;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Classes,
  Graphics,
  Forms,
  StdCtrls,
  RzCommon,
  RzFilSys;

type
  TRzSelDirDialog = class( TRzDialogComponent )
  private
    FAllowCreate: Boolean;
    FButtonGlyphs: Boolean;
    FChangeCurrentDir: Boolean;
    FDirectory: string;
    FNetworkVolumeFormat: TNetworkVolumeFormat;
    FOpenCurrentDir: Boolean;
    FAutoSelect: Boolean;
    FPrompt: string;
    FPromptFolders: string;
    FPromptDrives: string;
    FDriveTypes: TDriveTypes;
  public
    constructor Create( AOwner: TComponent ); override;
    function Execute: Boolean; dynamic;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AllowCreate: Boolean
      read FAllowCreate
      write FAllowCreate
      default False;

    property ButtonGlyphs: Boolean
      read FButtonGlyphs
      write FButtonGlyphs
      default False;

    property ChangeCurrentDir: Boolean
      read FChangeCurrentDir
      write FChangeCurrentDir
      default True;

    property Directory: string
      read FDirectory
      write FDirectory;

    property DriveTypes: TDriveTypes
      read FDriveTypes
      write FDriveTypes;

    property NetworkVolumeFormat: TNetworkVolumeFormat
      read FNetworkVolumeFormat
      write FNetworkVolumeFormat
      default nvfExplorer;

    property OpenCurrentDir: Boolean
      read FOpenCurrentDir
      write FOpenCurrentDir
      default False;

    property AutoSelect: Boolean
      read FAutoSelect
      write FAutoSelect
      default False;

    property Prompt: string
      read FPrompt
      write FPrompt;

    property PromptFolders: string
      read FPromptFolders
      write FPromptFolders;

    property PromptDrives: string
      read FPromptDrives
      write FPromptDrives;

    { Inherited Properties & Events }
    property BorderStyle default bsDialog;
    property Caption;
    property CaptionOK;
    property CaptionCancel;
    property CaptionHelp;
    property Font;
    property FrameColor;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property Height default 325;
    property HelpContext;
    property Width default 275;
  end deprecated;



implementation

uses
  {&RAS}
  Windows,
  Controls,
  RzSelDirForm,
  FileCtrl,
  SysUtils;

{&RT}
{=============================}
{== TRzSelDirDialog Methods ==}
{=============================}

constructor TRzSelDirDialog.Create( AOwner: TComponent );
begin
  inherited;

  FAllowCreate := False;
  Caption := 'Select a Folder';
  FChangeCurrentDir := True;
  FDirectory := '';
  FButtonGlyphs := False;
  BorderStyle := bsDialog;
  Height := 325;
  Width := 275;

  FDriveTypes := [ dtFloppy, dtFixed, dtNetwork, dtCDROM, dtRAM ];

  FPrompt := 'Folder Name';
  FPromptFolders := 'Folders';
  FPromptDrives := 'Drives';

  FOpenCurrentDir := False;
  FNetworkVolumeFormat := nvfExplorer;
  FAutoSelect := False;
  
  {&RCI}
end;


function TRzSelDirDialog.Execute: Boolean;
var
  OriginalDir: string;
  Dlg: TRzSelDirForm;
begin
  {&RV}
  if FDirectory = '' then
  begin
    OriginalDir := GetCurrentDir;
  end
  else
    OriginalDir := FDirectory;

  Dlg := TRzSelDirForm.Create( Application );
  with Dlg do
  begin
    try
      Dlg.BorderStyle := Self.BorderStyle;
      Dlg.Left := Self.OriginLeft;
      Dlg.Top := Self.OriginTop;
      Dlg.Width := Self.Width;
      Dlg.Height := Self.Height;

      CenterForm( Dlg );

      Dlg.Font := Self.Font;
      Dlg.Caption := Self.Caption;

      AllowCreate := FAllowCreate;
      LblDir.Visible := not FAllowCreate;
      EdtDir.Visible := FAllowCreate;

      if LblDir.Visible then
        LblDir.Height := GetMinFontHeight( Dlg.Font );

      if FAllowCreate then
        ActiveControl := EdtDir
      else
      begin
        ActiveControl := TvwDirs;
      end;

      PnlButtons.ShowHelpButton := Self.HelpContext <> 0;
      PnlButtons.HelpContext := Self.HelpContext;
      PnlButtons.ShowGlyphs := FButtonGlyphs;
      PnlButtons.CaptionOK := CaptionOK;
      PnlButtons.CaptionCancel := CaptionCancel;
      PnlButtons.CaptionHelp := CaptionHelp;

      LblPrompt.Caption := FPrompt;
      LblFolders.Caption := FPromptFolders;
      LblDrives.Caption := FPromptDrives;

      if FrameVisible then
      begin
        TvwDirs.FrameVisible := True;
        TvwDirs.FrameColor := FrameColor;
        TvwDirs.FrameStyle := FrameStyle;

        EdtDir.FrameVisible := True;
        EdtDir.FrameColor := FrameColor;
        EdtDir.FrameStyle := FrameStyle;
      end;

      TvwDirs.AutoSelect := FAutoSelect;
      TvwDirs.DriveTypes := FDriveTypes;
      if ( FDirectory = '' ) and FOpenCurrentDir then
        Directory := UNCPathToDriveMapping( OriginalDir )
      else
        Directory := UNCPathToDriveMapping( FDirectory );
      TvwDirs.NetworkVolumeFormat := FNetworkVolumeFormat;

      Result := ShowModal = idOK;
      if Result then
        FDirectory := Directory;

      if ( not Result or not FChangeCurrentDir ) and
         DirectoryExists( OriginalDir ) then
        ChDir( OriginalDir );

    finally
      Free;
    end;
  end; { with }
end; {= TRzSelDirDialog.Execute =}

{&RUIF}
end.
