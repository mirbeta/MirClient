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

unit cxShellDlgs;

interface

uses
  Windows, Messages, SysUtils, Classes,
  cxShellCommon, cxLookAndFeels;

type
  { TcxDlgShellOptions }

  TcxDlgShellOptions = class(TPersistent)
  private
    FContextMenus: Boolean;
    FShowFolders: Boolean;
    FShowHidden: Boolean;
    FShowNonFolders: Boolean;
    FShowZipFilesWithFolders: Boolean;
    FShowToolTip: Boolean;
    FTrackShellChanges: Boolean;
  public
    constructor Create; virtual;
  published
    property ContextMenus: Boolean read FContextMenus write fContextMenus
      default True;
    property ShowFolders: Boolean read FShowFolders write FShowFolders
      default True;
    property ShowHidden: Boolean read FShowHidden write fShowHidden
      default False;
    property ShowNonFolders: Boolean read FShowNonFolders write fShowNonFolders
      default False;
    property ShowToolTip: Boolean read FShowToolTip write FShowToolTip
      default True;
    property ShowZipFilesWithFolders: Boolean read FShowZipFilesWithFolders write FShowZipFilesWithFolders
      default True;
    property TrackShellChanges: Boolean read FTrackShellChanges
      write FTrackShellChanges default True;
  end;

  { TcxDlgShellRoot }

  TcxDlgShellRoot = class(TPersistent)
  private
    fCustomPath: string;
    fBroFold: TcxBrowseFolder;
  public
    constructor Create; virtual;
  published
    property BrowseFolder: TcxBrowseFolder read fBroFold write fBroFold default bfDesktop;
    property CustomPath: string read fCustomPath write fCustomPath;
  end;

implementation

{ TcxDdlgShellOptions }

constructor TcxDlgShellOptions.Create;
begin
  inherited Create;
  FContextMenus := True;
  FShowFolders := True;
  FShowToolTip := True;
  FTrackShellChanges := True;
  FShowZipFilesWithFolders := True;
end;

{ TcxDlgShellRoot }

constructor TcxDlgShellRoot.Create;
begin
  inherited;
  fBroFold := bfDesktop;
end;

end.
