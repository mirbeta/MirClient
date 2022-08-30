{=======================================================================================================================
  ShellFrame Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2002 by Raize Software, Inc.  All Rights Reserved.
=======================================================================================================================}

{$I RCDemo.inc}

unit ShellFrame;

interface

uses
  Forms,
  RzShellDialogs,
  ImgList,
  Controls,
  RzPanel,
  RzButton,
  ComCtrls,
  RzListVw,
  RzShellCtrls,
  StdCtrls,
  RzCmboBx,
  RzTreeVw,
  Classes,
  ExtCtrls,
  RzCommon,
  RzSplit;

type
  TFmeShell = class(TFrame)
    SplPreview: TRzSplitter;
    RzStatusBar1: TRzStatusBar;
    RzShellTree1: TRzShellTree;
    RzShellList1: TRzShellList;
    ImageList1: TImageList;
    RzOpenDialog1: TRzOpenDialog;
    RzSaveDialog1: TRzSaveDialog;
    RzSelectFolderDialog1: TRzSelectFolderDialog;
    pnlHeader: TRzPanel;
    RzToolbar1: TRzToolbar;
    BtnOpen: TRzToolButton;
    BtnSave: TRzToolButton;
    RzSpacer1: TRzSpacer;
    BtnUpOneLevel: TRzToolButton;
    BtnSelectFolder: TRzToolButton;
    RzSpacer2: TRzSpacer;
    BtnViewIcons: TRzToolButton;
    BtnViewList: TRzToolButton;
    BtnViewDetails: TRzToolButton;
    RzShellCombo1: TRzShellCombo;
    RzSpacer3: TRzSpacer;
    procedure BtnViewIconsClick(Sender: TObject);
    procedure BtnViewListClick(Sender: TObject);
    procedure BtnViewDetailsClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnUpOneLevelClick(Sender: TObject);
    procedure BtnSelectFolderClick(Sender: TObject);
    procedure RzShellTree1Change(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
  public
    procedure Init;
    procedure UpdateVisualStyle( VS: TRzVisualStyle; GCS: TRzGradientColorStyle );
  end;


implementation

{$R *.dfm}

procedure TFmeShell.Init;
begin
//  ParentBackground := False;
end;


procedure TFmeShell.UpdateVisualStyle( VS: TRzVisualStyle;
                                       GCS: TRzGradientColorStyle );
begin
  pnlHeader.VisualStyle := VS;
  pnlHeader.GradientColorStyle := GCS;
  RzToolbar1.VisualStyle := VS;
  RzToolbar1.GradientColorStyle := GCS;
  RzStatusBar1.VisualStyle := VS;
  RzStatusBar1.GradientColorStyle := GCS;
end;


procedure TFmeShell.BtnViewIconsClick(Sender: TObject);
begin
  RzShellList1.ViewStyle := vsIcon;
end;

procedure TFmeShell.BtnViewListClick(Sender: TObject);
begin
  RzShellList1.ViewStyle := vsList;
end;

procedure TFmeShell.BtnViewDetailsClick(Sender: TObject);
begin
  RzShellList1.ViewStyle := vsReport;
end;

procedure TFmeShell.BtnOpenClick(Sender: TObject);
begin
  RzOpenDialog1.Execute;
end;

procedure TFmeShell.BtnSaveClick(Sender: TObject);
begin
  RzSaveDialog1.Execute;
end;

procedure TFmeShell.BtnUpOneLevelClick(Sender: TObject);
begin
  RzShellTree1.GoUp( 1 );
end;

procedure TFmeShell.BtnSelectFolderClick(Sender: TObject);
begin
  if RzSelectFolderDialog1.Execute then
  begin
    RzShellTree1.SelectedFolder := RzSelectFolderDialog1.SelectedFolder;
  end;
end;

procedure TFmeShell.RzShellTree1Change(Sender: TObject; Node: TTreeNode);
begin
  RzStatusBar1.SimpleCaption := RzShellTree1.SelectedPathName;
end;

end.
