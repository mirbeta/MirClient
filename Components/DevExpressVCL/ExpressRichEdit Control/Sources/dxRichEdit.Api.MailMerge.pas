{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Api.MailMerge;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, DB,
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.MailMerge;

type
  { TdxNativeMailMergeOptions }

  TdxNativeMailMergeOptions = class(TInterfacedObject, IdxRichEditMailMergeOptions)
  strict private
    FDataSource: TDataSource;
    FMergeMode: TdxRichEditMergeMode;
    FCopyTemplateStyles: Boolean;
    FHeaderFooterLinkToPrevious: Boolean;
    FMergeRecords: TdxRichEditMergeRecords;
    //IdxRichEditMailMergeOptions
    function GetCopyTemplateStyles: Boolean;
    function GetDataSource: TDataSource;
    function GetHeaderFooterLinkToPrevious: Boolean;
    function GetMergeMode: TdxRichEditMergeMode;
    function GetMergeRecords: TdxRichEditMergeRecords;
    procedure SetCopyTemplateStyles(const Value: Boolean);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetHeaderFooterLinkToPrevious(const Value: Boolean);
    procedure SetMergeMode(const Value: TdxRichEditMergeMode);
    procedure SetMergeRecords(const Value: TdxRichEditMergeRecords);
  public
    constructor Create;
    function GetInternalMailMergeOptions: TdxMailMergeOptions;

    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property MergeMode: TdxRichEditMergeMode read GetMergeMode write SetMergeMode;
    property CopyTemplateStyles: Boolean read GetCopyTemplateStyles write SetCopyTemplateStyles;
    property HeaderFooterLinkToPrevious: Boolean read GetHeaderFooterLinkToPrevious write SetHeaderFooterLinkToPrevious;
    property MergeRecords: TdxRichEditMergeRecords read GetMergeRecords write SetMergeRecords;
  end;

implementation

{ TdxNativeMailMergeOptions }

constructor TdxNativeMailMergeOptions.Create;
begin
  inherited Create;
  FHeaderFooterLinkToPrevious := True;
end;

function TdxNativeMailMergeOptions.GetInternalMailMergeOptions: TdxMailMergeOptions;
begin
  Result := TdxMailMergeOptions.Create;
  Result.MergeMode := MergeMode;
  Result.DataSource := DataSource;
  Result.CopyTemplateStyles := CopyTemplateStyles;
  Result.HeaderFooterLinkToPrevious := HeaderFooterLinkToPrevious;
  Result.MergeRecords := MergeRecords;
end;

function TdxNativeMailMergeOptions.GetCopyTemplateStyles: Boolean;
begin
  Result := FCopyTemplateStyles;
end;

function TdxNativeMailMergeOptions.GetDataSource: TDataSource;
begin
  Result := FDataSource;
end;

function TdxNativeMailMergeOptions.GetHeaderFooterLinkToPrevious: Boolean;
begin
  Result := FHeaderFooterLinkToPrevious;
end;

function TdxNativeMailMergeOptions.GetMergeMode: TdxRichEditMergeMode;
begin
  Result := FMergeMode;
end;

function TdxNativeMailMergeOptions.GetMergeRecords: TdxRichEditMergeRecords;
begin
  Result := FMergeRecords;
end;

procedure TdxNativeMailMergeOptions.SetCopyTemplateStyles(
  const Value: Boolean);
begin
  FCopyTemplateStyles := Value;
end;

procedure TdxNativeMailMergeOptions.SetDataSource(const Value: TDataSource);
begin
  FDataSource := Value;
end;

procedure TdxNativeMailMergeOptions.SetHeaderFooterLinkToPrevious(
  const Value: Boolean);
begin
  FHeaderFooterLinkToPrevious := Value;
end;

procedure TdxNativeMailMergeOptions.SetMergeMode(
  const Value: TdxRichEditMergeMode);
begin
  FMergeMode := Value;
end;

procedure TdxNativeMailMergeOptions.SetMergeRecords(const Value: TdxRichEditMergeRecords);
begin
  FMergeRecords := Value;
end;

end.
