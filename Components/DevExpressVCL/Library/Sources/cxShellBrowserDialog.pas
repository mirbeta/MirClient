{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxShellBrowserDialog;

{$I cxVer.Inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, ShlObj,
  cxClasses, cxShellCommon, cxShellControls, cxShellDlgs, cxShellBrowser, cxLookAndFeels;

type
  { TcxCustomShellBrowserDialog }

  TcxCustomShellBrowserDialog = class(TcxCustomComponent, IdxSkinSupport)
  private
    FAbsolutePIDL: PItemIDList;
    FFolderCaption: string;
    FLookAndFeel: TcxLookAndFeel;
    FPath: string;
    FRoot: TcxDlgShellRoot;
    FShellOptions: TcxDlgShellOptions;
    FShowButtons: Boolean;
    FShowInfoTips: Boolean;
    FShowLines: Boolean;
    FShowRoot: Boolean;
    FTitle: string;
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
  protected
    function CreateForm: TcxShellBrowserDlg; dynamic;
    function CreateShellOptions: TcxDlgShellOptions; virtual;
    function CreateShellRoot: TcxDlgShellRoot; virtual;
    property AbsolutePIDL: PItemIDList read FAbsolutePIDL;
    property FolderLabelCaption: string read FFolderCaption write FFolderCaption;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property Options: TcxDlgShellOptions read FShellOptions write FShellOptions;
    property Path: string read FPath write FPath;
    property Root: TcxDlgShellRoot read FRoot write FRoot;
    property ShowButtons: Boolean read FShowButtons write FShowButtons default True;
    property ShowInfoTips: Boolean read FShowInfoTips write FShowInfoTips default False;
    property ShowLines: Boolean read FShowLines write FShowLines default True;
    property ShowRoot: Boolean read FShowRoot write FShowRoot default True;
    property Title: string read FTitle write FTitle;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  end;

  { TcxShellBrowserDialog }

  TcxShellBrowserDialog = class(TcxCustomShellBrowserDialog)
  public
    property AbsolutePIDL;
  published
    property FolderLabelCaption;
    property LookAndFeel;
    property Options;
    property Path;
    property Root;
    property ShowButtons;
    property ShowInfoTips;
    property ShowLines;
    property ShowRoot;
    property Title;
  end;

implementation

{ TcxCustomShellBrowser }

constructor TcxCustomShellBrowserDialog.Create(aOwner: TComponent);
begin
  inherited;
  FShellOptions := CreateShellOptions;
  FRoot := CreateShellRoot;
  Title := '';
  FShowRoot := True;
  FShowButtons := True;
  FShowLines := True;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
end;

destructor TcxCustomShellBrowserDialog.Destroy;
begin
  DisposePidl(FAbsolutePIDL);
  FAbsolutePIDL := nil;
  FreeAndNil(FLookAndFeel);
  FRoot.Free;
  FShellOptions.Free;
  inherited;
end;

function TcxCustomShellBrowserDialog.Execute: Boolean;
var
  APreviousCursor: TCursor;
  ADlg: TcxShellBrowserDlg;
begin
  DisposePidl(FAbsolutePIDL);
  FAbsolutePIDL := nil;
  APreviousCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    ADlg := CreateForm;
    try
      if Length(Title) > 0 then
        ADlg.DlgCaption := Title;
      if Length(FFolderCaption) > 0 then
        ADlg.DlgFolderLabelCaption := FFolderCaption;
      ADlg.DlgOptions.ShowFolders := FShellOptions.ShowFolders;
      ADlg.DlgOptions.ShowToolTip := FShellOptions.ShowToolTip;
      ADlg.DlgOptions.TrackShellChanges := FShellOptions.TrackShellChanges;
      ADlg.DlgOptions.ContextMenus := FShellOptions.ContextMenus;
      ADlg.DlgOptions.ShowNonFolders := FShellOptions.ShowNonFolders;
      ADlg.DlgOptions.ShowHidden := FShellOptions.ShowHidden;
      ADlg.DlgOptions.ShowZipFilesWithFolders := FShellOptions.ShowZipFilesWithFolders;
      ADlg.DlgRoot.BrowseFolder := FRoot.BrowseFolder;
      ADlg.DlgRoot.CustomPath := FRoot.CustomPath;
      ADlg.DlgShowButtons := ShowButtons;
      ADlg.DlgShowInfoTips := ShowInfoTips;
      ADlg.DlgShowLines := ShowLines;
      ADlg.DlgShowRoot := ShowRoot;
      ADlg.DlgFolder := fPath;
      ADlg.LookAndFeel.MasterLookAndFeel := Self.LookAndFeel;
      Result := ADlg.ShowModal = idOk;
      if Result then
      begin
        FPath := ADlg.DlgFolder;
        FAbsolutePIDL := ADlg.AbsolutePIDL;
      end;
    finally
      FreeAndNil(ADlg);
    end;
  finally
    Screen.Cursor := APreviousCursor;
  end;
end;

function TcxCustomShellBrowserDialog.CreateForm: TcxShellBrowserDlg;
begin
  Result := TcxShellBrowserDlg.Create(Application);
end;

function TcxCustomShellBrowserDialog.CreateShellOptions: TcxDlgShellOptions;
begin
  Result := TcxDlgShellOptions.Create;
end;

function TcxCustomShellBrowserDialog.CreateShellRoot: TcxDlgShellRoot;
begin
  Result := TcxDlgShellRoot.Create;
end;

procedure TcxCustomShellBrowserDialog.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

end.
