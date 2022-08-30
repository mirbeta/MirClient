{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY                                          }
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

unit dxPScxDBEditorLnks;

interface

{$I cxVer.inc}

uses
  cxDBEdit, dxDBBarCode, dxPScxEditorLnks;

type
  TcxDBListBoxReportLink = class(TcxListBoxReportLink)
  private
    function GetcxDBListBox: TcxDBListBox;
  public
    property cxDBListBox: TcxDBListBox read GetcxDBListBox;
  end;

  TcxDBImageReportLink = class(TcxCustomImageReportLink)
  private
    function GetcxDBImage: TcxDBImage;
  public
    property cxDBImage: TcxDBImage read GetcxDBImage;
  published
    property BorderColor;
    property Center;
    property DrawBorder;
    property Stretch;
    property Transparent;
    property TransparentColor;
  end;

  TcxDBMemoReportLink = class(TcxCustomMemoReportLink)
  private
    function GetcxDBMemo: TcxDBMemo;
  public
    property cxDBMemo: TcxDBMemo read GetcxDBMemo;
  end;

  TdxDBBarCodeReportLink = class(TdxCustomBarCodeReportLink)
  private
    function GetdxDBBarCode: TdxDBBarCode;
  public
    property dxDBBarCode: TdxDBBarCode read GetdxDBBarCode;
  published
    property BorderColor;
    property DrawBorder;
    property FitMode;
  end;

implementation

uses
  dxPSCore, dxPSLbxLnk, dxPSTextLnk;

{ TcxDBListBoxReportLink }

function TcxDBListBoxReportLink.GetcxDBListBox: TcxDBListBox;
begin
  Result := inherited Component as TcxDBListBox;
end;

{ TcxDBImageReportLink }

function TcxDBImageReportLink.GetcxDBImage: TcxDBImage;
begin
  Result := inherited Component as TcxDBImage;
end;

{ TcxDBMemoReportLink }

function TcxDBMemoReportLink.GetcxDBMemo: TcxDBMemo;
begin
  Result := inherited Component as TcxDBMemo;
end;

{ TdxDBBarCodeReportLink }

function TdxDBBarCodeReportLink.GetdxDBBarCode: TdxDBBarCode;
begin
  Result := inherited Component as TdxDBBarCode;
end;

initialization
  dxPSRegisterReportLink(TcxDBListBoxReportLink, TcxDBListBox, dxPSLbxLnk.TdxLBxReportLinkDesignWindow);
  dxPSRegisterReportLink(TcxDBImageReportLink, TcxDBImage, nil);
  dxPSRegisterReportLink(TcxDBMemoReportLink, TcxDBMemo, TdxfmTextReportLinkDesignWindow);
  dxPSRegisterReportLink(TdxDBBarCodeReportLink, TdxDBBarCode, nil);

finalization
  dxPSUnregisterReportLink(TdxDBBarCodeReportLink, TdxDBBarCode, nil);
  dxPSUnregisterReportLink(TcxDBMemoReportLink, TcxDBMemo, TdxfmTextReportLinkDesignWindow);
  dxPSUnregisterReportLink(TcxDBImageReportLink, TcxDBImage, nil);
  dxPSUnregisterReportLink(TcxDBListBoxReportLink, TcxDBListBox, dxPSLbxLnk.TdxLBxReportLinkDesignWindow);

end.

