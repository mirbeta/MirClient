{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
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

unit dxServerModeUtils;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, cxCustomData, DB, dxServerModeClasses;

function dxCreateServerModeRow(AFields: TFields): TdxServerModeRow;
function dxGetServerModeRowValueCount(const ARow: TdxServerModeRow): Integer;
function dxVarArrayToDelimitedText(const AValue: Variant; const ADelimiter, AQuote: string): string;
function dxVarArrayToDelimetedText(const AValue: Variant; const ADelimiter, AQuote: string): string; deprecated 'use the dxVarArrayToDelimitedText global function instead'
function dxKeyToText(const AKey: Variant): string;
function dxServerModeGetFieldValue(AField: TField): Variant;

implementation

uses
  Variants, cxVariants;

function dxServerModeGetFieldValue(AField: TField): Variant;
begin
  if AField.IsNull then
    Result := Null
  else
    Result := AField.Value;
end;

function dxCreateServerModeRow(AFields: TFields): TdxServerModeRow;
var
  I: Integer;
begin
  if (AFields = nil) or (AFields.Count = 0) then
    Result := NULL
  else
    if AFields.Count = 1 then
      Result := dxServerModeGetFieldValue(AFields[0])
    else
    begin
      Result := VarArrayCreate([0, AFields.Count - 1], varVariant);
      for I := 0 to AFields.Count - 1 do
        Result[I] := dxServerModeGetFieldValue(AFields[I]);
    end;
end;

function dxGetServerModeRowValueCount(const ARow: TdxServerModeRow): Integer;
begin
  if VarIsArray(ARow) then
    Result := VarArrayHighBound(ARow, 1) - VarArrayLowBound(ARow, 1) + 1
  else
    Result := 1;
end;

function dxVarArrayToDelimitedText(const AValue: Variant; const ADelimiter, AQuote: string): string;

  function InternalVarToStr(const AValue: Variant): string;
  begin
    if VarIsStr(AValue) then
      Result := Format('%s%s%s', [AQuote, VarToStr(AValue), AQuote])
    else
      Result := VarToStr(AValue);
  end;

var
  I: Integer;
begin
  if VarIsArray(AValue) then
  begin
    Result := '';
    for I := VarArrayLowBound(AValue, 1) to VarArrayHighBound(AValue, 1) do
    begin
      if Length(Result) > 0 then
        Result := Result + ADelimiter;
      Result := Result + dxVarArrayToDelimitedText(AValue[I], ADelimiter, AQuote);
    end;
  end
  else
    Result := InternalVarToStr(AValue);
end;

function dxVarArrayToDelimetedText(const AValue: Variant; const ADelimiter, AQuote: string): string;
begin
  Result := dxVarArrayToDelimitedText(AValue, ADelimiter, AQuote);
end;

function dxKeyToText(const AKey: Variant): string;
begin
  Result := dxVarArrayToDelimitedText(AKey , ',', '');
end;

end.
