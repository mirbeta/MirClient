{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPageControl                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPAGECONTROL AND ALL            }
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

unit cxPCPaintersFactory;

{$I cxVer.inc}

interface

uses
  Types, Classes, Controls, SysUtils, dxCore, cxClasses, cxLookAndFeels, cxPC, cxPCConsts;

type

  TcxPCPaintersFactory = class
  private
    FPainterClassList: TList;
    class procedure OutError(const SourceMethodName, Msg: string);
    function GetPainterClassCount: Integer;
    function GetPainterStyleID(Index: Integer): Integer;
    function InternalGetPainterClass(Index: Integer): TcxPCPainterClass;
  public
    constructor Create;
    destructor Destroy; override;

    function GetDefaultStyleID(ALookAndFeel: TcxLookAndFeel): TcxPCStyleID;
    function GetPainterClass(const StandardStyle: TcxPCStandardStyle): TcxPCPainterClass; overload;
    function GetPainterClass(const StyleID: TcxPCStyleID): TcxPCPainterClass; overload;
    function GetPainterClass(const StyleName: TCaption): TcxPCPainterClass; overload;

    function GetStyleID(ALookAndFeel: TcxLookAndFeel): TcxPCStyleID;

    procedure RegisterPCPainterClass(PCPainterClass: TcxPCPainterClass);
    procedure UnregisterPCPainterClass(PCPainterClass: TcxPCPainterClass);

    property PainterClassCount: Integer read GetPainterClassCount;
    property PainterClasses[Index: Integer]: TcxPCPainterClass read InternalGetPainterClass;
    property PainterStyleIDs[Index: Integer]: Integer read GetPainterStyleID;
  end;

function PaintersFactory: TcxPCPaintersFactory;
procedure RegisterPCPainterClass(PCPainterClass: TcxPCPainterClass);
procedure UnregisterPCPainterClass(PCPainterClass: TcxPCPainterClass);

implementation

var
  FPaintersFactory: TcxPCPaintersFactory = nil;

function PaintersFactory: TcxPCPaintersFactory;
begin
  Result := FPaintersFactory;
end;

procedure RegisterPCPainterClass(PCPainterClass: TcxPCPainterClass);
begin
  if FPaintersFactory = nil then
    FPaintersFactory := TcxPCPaintersFactory.Create;
  FPaintersFactory.RegisterPCPainterClass(PCPainterClass);
end;

procedure UnregisterPCPainterClass(PCPainterClass: TcxPCPainterClass);
begin
  if FPaintersFactory <> nil then
  begin
    FPaintersFactory.UnregisterPCPainterClass(PCPainterClass);
    if FPaintersFactory.PainterClassCount = 0 then
      FreeAndNil(FPaintersFactory);
  end;
end;

{ TcxPCPaintersFactory }

constructor TcxPCPaintersFactory.Create;
begin
  inherited Create;
  FPainterClassList := TList.Create;
end;

destructor TcxPCPaintersFactory.Destroy;
begin
  FPainterClassList.Free;
  inherited Destroy;
end;

function TcxPCPaintersFactory.GetDefaultStyleID(ALookAndFeel: TcxLookAndFeel): TcxPCStyleID;
var
  I: Integer;
begin
  if PainterClassCount = 0 then
    OutError('GetDefaultStyleID', cxGetResourceString(@scxPCNoRegisteredStyles));
  Result := PainterClasses[0].GetStyleID;
  for I := PainterClassCount - 1 downto 0 do
    if PainterClasses[I].IsDefault(ALookAndFeel) then
    begin
      Result := PainterClasses[I].GetStyleID;
      Break;
    end;
end;

function TcxPCPaintersFactory.GetPainterClass(const StandardStyle: TcxPCStandardStyle):
  TcxPCPainterClass;
var
  I: Integer;
begin
  for I := 0 to PainterClassCount - 1 do
  begin
    Result := PainterClasses[I];
    if Result.IsStandardStyle and (Result.GetStandardStyle = StandardStyle) then
      Exit;
  end;
  Result := nil;
end;

function TcxPCPaintersFactory.GetPainterClass(const StyleID: TcxPCStyleID): TcxPCPainterClass;
var
  I: Integer;
begin
  for I := 0 to PainterClassCount - 1 do
  begin
    Result := PainterClasses[I];
    if Result.GetStyleID = StyleID then Exit;
  end;
  Result := nil;
end;

function TcxPCPaintersFactory.GetPainterClass(const StyleName: TCaption): TcxPCPainterClass;
var
  I: Integer;
begin
  for I := 0 to PainterClassCount - 1 do
  begin
    Result := PainterClasses[I];
    if AnsiUpperCase(Result.GetStyleName) = AnsiUpperCase(StyleName) then
      Exit;
  end;
  Result := nil;
end;

function TcxPCPaintersFactory.GetPainterClassCount: Integer;
begin
  Result := FPainterClassList.Count;
end;

function TcxPCPaintersFactory.GetPainterStyleID(Index: Integer): Integer;
begin
  Result := PainterClasses[Index].GetStyleID;
end;

function TcxPCPaintersFactory.GetStyleID(ALookAndFeel: TcxLookAndFeel): TcxPCStyleID;
var
  I: Integer;
begin
  Result := cxPCNoStyle;
  for I := PainterClassCount - 1 downto 0 do
    if PainterClasses[I].HasLookAndFeel(ALookAndFeel) then
    begin
      Result := PainterClasses[I].GetStyleID;
      Break;
    end;
end;

function TcxPCPaintersFactory.InternalGetPainterClass(Index: Integer): TcxPCPainterClass;
begin
  Result := TcxPCPainterClass(FPainterClassList.Items[Index]);
end;

class procedure TcxPCPaintersFactory.OutError(const SourceMethodName, Msg: string);
begin
  raise EdxException.Create('TcxPCPaintersFactory.' + SourceMethodName + ': ' + Msg);
end;

procedure TcxPCPaintersFactory.RegisterPCPainterClass(
  PCPainterClass: TcxPCPainterClass);
begin
  if FPainterClassList.IndexOf(TObject(PCPainterClass)) = -1 then
    FPainterClassList.Add(TObject(PCPainterClass));
end;

procedure TcxPCPaintersFactory.UnregisterPCPainterClass(
  PCPainterClass: TcxPCPainterClass);
begin
  FPainterClassList.Remove(TObject(PCPainterClass));
end;

initialization

finalization
  FreeAndNil(FPaintersFactory);

end.
