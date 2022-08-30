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

unit dxRichEdit.Import.Doc.DocRangeEditPermission;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.Doc.DocStringTable,
  dxRichEdit.Import.Doc.FileInformationBlock,
  dxRichEdit.Import.Doc.DocBookmark,
  dxRichEdit.Import.Doc.PositionConverter;

type
   TdxDocUserRole = class
   public const
     None   = $0000;
     Owner  = $fffc;
     Editor = $fffb;
   end;

  { TdxDocProtectionInfo }

  TdxDocProtectionInfo = class
  public const
    IgnoredDataSize = Integer(4);
  strict private
    FProtectionType: TdxDocumentProtectionType;
    FUid: Word;
  protected
    procedure Read(AReader: TBinaryReader); virtual;

    property Uid: Word read FUid write FUid;
  public
    constructor Create; overload;
    constructor Create(AUid: Integer; AProtectionType: TdxDocumentProtectionType); overload;
    class function FromStream(AReader: TBinaryReader): TdxDocProtectionInfo; static;
    procedure Write(AWriter: TBinaryWriter);

    property ProtectionType: TdxDocumentProtectionType read FProtectionType write FProtectionType;
  end;

  { TdxUserProtectionStringTable }

  TdxUserProtectionStringTable = class(TdxDocStringTableBase)
  public const
    ExtraDataSize = SmallInt($02);
    OwnersId      = Integer($fffc);
    EditorsId     = Integer($fffb);
    EveryoneId    = Integer($ffff);
  strict private
    FUsers: TdxStringList;
    FRoles: TList<Word>;
    FEditors: TdxStringList;
    FOwners: TdxStringList;
  protected
    function CalcExtraDataSize(AReader: TBinaryReader): Integer; override;
    function CalcRecordsCount(AReader: TBinaryReader): Integer; override;
    procedure WriteExtraDataSize(AWriter: TBinaryWriter); override;
    procedure WriteCount(AWriter: TBinaryWriter); override;
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer); override;
    procedure ReadString(AReader: TBinaryReader); override;
    procedure ReadExtraData(AReader: TBinaryReader); override;
    procedure GroupUsersByRole;
    procedure WriteString(AWriter: TBinaryWriter; AIndex: Integer); override;
    procedure WriteExtraData(AWriter: TBinaryWriter; AIndex: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxUserProtectionStringTable; static;
    function GetUsersById(AId: Integer): TdxStringList;
    function GetUserIdByName(const AUserName: string): Integer;
    procedure Write(AWriter: TBinaryWriter); override;

    property Users: TdxStringList read FUsers write FUsers;
    property Roles: TList<Word> read FRoles write FRoles;
  end;

  { TdxRangeEditPermissionsStringTable }

  TdxRangeEditPermissionsStringTable = class(TdxDocStringTableBase)
  public const
    ProtectionInfoSize = SmallInt($8);
  strict private
    FProtectionInfos: TdxObjectList<TdxDocProtectionInfo>;
  protected
    function CalcExtraDataSize(AReader: TBinaryReader): Integer; override;
    function CalcRecordsCount(AReader: TBinaryReader): Integer; override;
    procedure WriteExtraDataSize(AWriter: TBinaryWriter); override;
    procedure WriteCount(AWriter: TBinaryWriter); override;
    procedure ReadString(AReader: TBinaryReader); override;
    procedure ReadExtraData(AReader: TBinaryReader); override;
    procedure WriteString(AWriter: TBinaryWriter; AIndex: Integer); override;
    procedure WriteExtraData(AWriter: TBinaryWriter; AIndex: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxRangeEditPermissionsStringTable; static;
    procedure Write(AWriter: TBinaryWriter); override;

    property ProtectionInfos: TdxObjectList<TdxDocProtectionInfo> read FProtectionInfos write FProtectionInfos;
  end;

  { TdxDocImportRangePermissionInfo }

  TdxDocImportRangePermissionInfo = class
  strict private
    FPermissionInfo: TdxRangePermissionInfo;
    FOriginalStartPosition: Integer;
    FOriginalEndPosition: Integer;
    FProtectionType: TdxDocumentProtectionType;
  public
    constructor Create(AOriginalStartPosition: Integer; AOriginalEndPosition: Integer);
    destructor Destroy; override;
    function Validate: Boolean;

    property OriginalStartPosition: Integer read FOriginalStartPosition;
    property OriginalEndPosition: Integer read FOriginalEndPosition;
    property PermissionInfo: TdxRangePermissionInfo read FPermissionInfo;
    property ProtectionType: TdxDocumentProtectionType read FProtectionType write FProtectionType;
  end;

  { TdxDocRangeEditPermissionIterator }

  TdxDocRangeEditPermissionIterator = class(TdxDocBookmarkIteratorBase)
  public const
    BookmarkIndexSize = Integer(4);
    DefaultGroupName = 'everyone';
  strict private
    FKeepPermissionsForRemovedRanges: Boolean;
    FProtectionInfos: TdxRangeEditPermissionsStringTable;
    FUsers: TdxUserProtectionStringTable;
    FPermissions: TdxObjectList<TdxDocImportRangePermissionInfo>;
    FPermissionStartPositions: TdxIntegersDictionary;
  protected
    procedure Read(AFib: TdxFileInformationBlock; AReader: TBinaryReader); override;
    procedure InitRangeEditPermissions;
    function CreatePermissionInfos(AStart: Integer; AEnd: Integer; AType: TdxDocumentProtectionType;
      AUsers: TdxStringList): TdxObjectList<TdxDocImportRangePermissionInfo>;
  public
    constructor Create; overload;
    constructor Create(AFib: TdxFileInformationBlock; AReader: TBinaryReader; AKeepPermissionsForRemovedRanges: Boolean); overload;
    destructor Destroy; override;
    procedure InsertRangeEditPermissions(APieceTable: TdxPieceTable);
    procedure RemoveProcessedPermissions(AProcessedPermissions: TdxList<TdxDocImportRangePermissionInfo>);
    procedure Write(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter); override;
    procedure AddPermissionStart(APermission: TdxRangePermission; AStartPosition: Integer);
    procedure AddPermissionEnd(APermission: TdxRangePermission; AEndPosition: Integer);
  end;

implementation

uses
  Types, Contnrs,
  dxStringHelper,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocImporter;

{ TdxDocProtectionInfo }

constructor TdxDocProtectionInfo.Create(AUid: Integer; AProtectionType: TdxDocumentProtectionType);
begin
  FUid := Word(AUid);
  ProtectionType := AProtectionType;
end;

constructor TdxDocProtectionInfo.Create;
begin
end;

class function TdxDocProtectionInfo.FromStream(AReader: TBinaryReader): TdxDocProtectionInfo;
begin
  Result := TdxDocProtectionInfo.Create;
  Result.Read(AReader);
end;

procedure TdxDocProtectionInfo.Read(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(IgnoredDataSize, TSeekOrigin.soCurrent);
  FUid := AReader.ReadUInt16;
  ProtectionType := TdxDocumentProtectionTypeCalculator.CalcRangePermissionProtectionType(AReader.ReadSmallInt);
  AReader.BaseStream.Seek(IgnoredDataSize, TSeekOrigin.soCurrent);
end;

procedure TdxDocProtectionInfo.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(FUid);
  AWriter.Write(TdxDocumentProtectionTypeCalculator.CalcRangePermissionProtectionTypeCode(ProtectionType));
  AWriter.BaseStream.Seek(IgnoredDataSize, TSeekOrigin.soCurrent);
end;

{ TdxUserProtectionStringTable }

constructor TdxUserProtectionStringTable.Create;
begin
  FUsers := TdxStringList.Create;
  FRoles := TList<Word>.Create;
end;

destructor TdxUserProtectionStringTable.Destroy;
begin
  FUsers.Free;
  FRoles.Free;
  FEditors.Free;
  FOwners.Free;
  inherited Destroy;
end;

class function TdxUserProtectionStringTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxUserProtectionStringTable;
begin
  Result := TdxUserProtectionStringTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

function TdxUserProtectionStringTable.GetUsersById(AId: Integer): TdxStringList;
begin
  if AId = EditorsId then
    Exit(FEditors);
  if AId = OwnersId then
    Exit(FOwners);
  if (AId > 0) and (AId <= Users.Count) then
  begin
    Result := TdxStringList.Create;
    Result.Add(Users[AId - 1]);
  end
  else
    Result := nil;
end;

function TdxUserProtectionStringTable.GetUserIdByName(const AUserName: string): Integer;
var
  ACount, I: Integer;
begin
  if AUserName = '' then
    Exit(EveryoneId);

  ACount := Users.Count;
  for I := 0 to ACount - 1 do
  begin
    if Users[I] = AUserName then
      Exit(I + 1);
  end;
  Users.Add(AUserName);
  Roles.Add(TdxDocUserRole.None);
  Result := Users.Count;
end;

function TdxUserProtectionStringTable.CalcExtraDataSize(AReader: TBinaryReader): Integer;
begin
  if AReader.ReadSmallInt <> ExtraDataSize then
    TdxDocImporter.ThrowInvalidDocFile;
  Result := ExtraDataSize;
end;

function TdxUserProtectionStringTable.CalcRecordsCount(AReader: TBinaryReader): Integer;
begin
  Result := AReader.ReadSmallInt;
end;

procedure TdxUserProtectionStringTable.WriteExtraDataSize(AWriter: TBinaryWriter);
begin
  AWriter.Write(ExtraDataSize);
end;

procedure TdxUserProtectionStringTable.WriteCount(AWriter: TBinaryWriter);
begin
  AWriter.Write(SmallInt(Count));
end;

procedure TdxUserProtectionStringTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
begin
  inherited Read(AReader, AOffset, ASize);
  GroupUsersByRole;
end;

procedure TdxUserProtectionStringTable.ReadString(AReader: TBinaryReader);
var
  ALength: Integer;
  ABuffer: TBytes;
begin
  ALength := AReader.ReadSmallInt * 2;
  ABuffer := AReader.ReadBytes(ALength);
  Users.Add(TdxStringHelper.RemoveSpecialSymbols(Encoding.GetString(ABuffer)));
end;

procedure TdxUserProtectionStringTable.ReadExtraData(AReader: TBinaryReader);
begin
  Roles.Add(AReader.ReadUInt16);
end;

procedure TdxUserProtectionStringTable.GroupUsersByRole;
var
  ACount, I: Integer;
begin
  Assert(Users.Count = Roles.Count);
  FEditors.Free;
  FEditors := TdxStringList.Create;
  FOwners.Free;
  FOwners := TdxStringList.Create;
  ACount := Users.Count;
  for I := 0 to ACount - 1 do
  begin
    if Roles[I] = TdxDocUserRole.Editor then
      FEditors.Add(Users[I])
    else
      if Roles[I] = TdxDocUserRole.Owner then
        FOwners.Add(Users[I]);
  end;
end;

procedure TdxUserProtectionStringTable.Write(AWriter: TBinaryWriter);
begin
  if Users.Count = 0 then
    Exit;
  Count := Users.Count;
  inherited Write(AWriter);
end;

procedure TdxUserProtectionStringTable.WriteString(AWriter: TBinaryWriter; AIndex: Integer);
var
  AUser: string;
begin
  AUser := Users[AIndex];
  AWriter.Write(SmallInt(Length(AUser)));
  AWriter.Write(GetEncoding.GetBytes(AUser));
end;

procedure TdxUserProtectionStringTable.WriteExtraData(AWriter: TBinaryWriter; AIndex: Integer);
begin
  AWriter.Write(Roles[AIndex]);
end;

{ TdxRangeEditPermissionsStringTable }

constructor TdxRangeEditPermissionsStringTable.Create;
begin
  FProtectionInfos := TdxObjectList<TdxDocProtectionInfo>.Create;
end;

destructor TdxRangeEditPermissionsStringTable.Destroy;
begin
  FProtectionInfos.Free;
  inherited Destroy;
end;

class function TdxRangeEditPermissionsStringTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxRangeEditPermissionsStringTable;
begin
  Result := TdxRangeEditPermissionsStringTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

function TdxRangeEditPermissionsStringTable.CalcExtraDataSize(AReader: TBinaryReader): Integer;
begin
  if AReader.ReadSmallInt <> ProtectionInfoSize then
    TdxDocImporter.ThrowInvalidDocFile;
  Result := ProtectionInfoSize;
end;

function TdxRangeEditPermissionsStringTable.CalcRecordsCount(AReader: TBinaryReader): Integer;
begin
  Result := AReader.ReadInt32;
end;

procedure TdxRangeEditPermissionsStringTable.Write(AWriter: TBinaryWriter);
begin
  Count := ProtectionInfos.Count;
  if Count > 0 then
    inherited Write(AWriter);
end;

procedure TdxRangeEditPermissionsStringTable.WriteExtraDataSize(AWriter: TBinaryWriter);
begin
  AWriter.Write(ProtectionInfoSize);
end;

procedure TdxRangeEditPermissionsStringTable.WriteCount(AWriter: TBinaryWriter);
begin
  AWriter.Write(Count);
end;

procedure TdxRangeEditPermissionsStringTable.ReadString(AReader: TBinaryReader);
begin
  AReader.ReadSmallInt;
end;

procedure TdxRangeEditPermissionsStringTable.ReadExtraData(AReader: TBinaryReader);
begin
  ProtectionInfos.Add(TdxDocProtectionInfo.FromStream(AReader));
end;

procedure TdxRangeEditPermissionsStringTable.WriteString(AWriter: TBinaryWriter; AIndex: Integer);
begin
  AWriter.Write(SmallInt($00));
end;

procedure TdxRangeEditPermissionsStringTable.WriteExtraData(AWriter: TBinaryWriter; AIndex: Integer);
begin
  ProtectionInfos[AIndex].Write(AWriter);
end;

{ TdxDocImportRangePermissionInfo }

constructor TdxDocImportRangePermissionInfo.Create(AOriginalStartPosition: Integer; AOriginalEndPosition: Integer);
begin
  FPermissionInfo := TdxRangePermissionInfo.Create;
  FOriginalStartPosition := AOriginalStartPosition;
  FOriginalEndPosition := AOriginalEndPosition;
end;

destructor TdxDocImportRangePermissionInfo.Destroy;
begin
  FPermissionInfo.Free;
  inherited Destroy;
end;

function TdxDocImportRangePermissionInfo.Validate: Boolean;
begin
  if (PermissionInfo.UserName = '') and (PermissionInfo.Group = '') then
    Exit(False);
  Result := OriginalStartPosition <= OriginalEndPosition;
end;

{ TdxDocRangeEditPermissionIterator }

constructor TdxDocRangeEditPermissionIterator.Create(AFib: TdxFileInformationBlock; AReader: TBinaryReader; AKeepPermissionsForRemovedRanges: Boolean);
begin
  inherited Create(AFib, AReader);
  FKeepPermissionsForRemovedRanges := AKeepPermissionsForRemovedRanges;
end;

constructor TdxDocRangeEditPermissionIterator.Create;
begin
  inherited Create;
  FPermissionStartPositions := TdxIntegersDictionary.Create;
  FProtectionInfos := TdxRangeEditPermissionsStringTable.Create;
  FUsers := TdxUserProtectionStringTable.Create;
end;

destructor TdxDocRangeEditPermissionIterator.Destroy;
begin
  FPermissionStartPositions.Free;
  FPermissions.Free;
  FProtectionInfos.Free;
  FUsers.Free;
  inherited Destroy;
end;

procedure TdxDocRangeEditPermissionIterator.InsertRangeEditPermissions(APieceTable: TdxPieceTable);
var
  I: Integer;
  AProcessedPermissions: TdxList<TdxDocImportRangePermissionInfo>;
  AStart, AEnd: TdxDocumentLogPosition;
  ASkipDeletedPositions, AStartPositionObtainable, AEndPositionObtainable: Boolean;
  AInfo: TdxDocImportRangePermissionInfo;
begin
  AProcessedPermissions := TdxList<TdxDocImportRangePermissionInfo>.Create;
  try
    ASkipDeletedPositions := not FKeepPermissionsForRemovedRanges;
    for I := 0 to FPermissions.Count - 1 do
    begin
      AInfo := FPermissions[I];
      if (not PositionConverter.ContainsPosition(AInfo.OriginalStartPosition)) or (not PositionConverter.ContainsPosition(AInfo.OriginalEndPosition)) or (not AInfo.Validate) then
        Continue;
      AStartPositionObtainable := PositionConverter.TryConvert(AInfo.OriginalStartPosition, ASkipDeletedPositions, AStart);
      AEndPositionObtainable := PositionConverter.TryConvert(AInfo.OriginalEndPosition, ASkipDeletedPositions, AEnd);
      if (AStartPositionObtainable and AEndPositionObtainable) and AInfo.Validate then
      begin
        APieceTable.ApplyDocumentPermission(AStart, AEnd, AInfo.PermissionInfo);
        AProcessedPermissions.Add(AInfo);
        Continue;
      end;
      if not AStartPositionObtainable then
        PositionConverter.TryConvert(AInfo.OriginalStartPosition, False, AStart);
      if not AEndPositionObtainable then
        PositionConverter.TryConvert(AInfo.OriginalEndPosition, False, AEnd);
      if AEnd > AStart then
      begin
        APieceTable.ApplyDocumentPermission(AStart, AEnd, AInfo.PermissionInfo);
        AProcessedPermissions.Add(AInfo);
      end;
    end;
    RemoveProcessedPermissions(AProcessedPermissions);
  finally
    AProcessedPermissions.Free;
  end;
end;

procedure TdxDocRangeEditPermissionIterator.RemoveProcessedPermissions(AProcessedPermissions: TdxList<TdxDocImportRangePermissionInfo>);
var
  ACount, I: Integer;
begin
  ACount := AProcessedPermissions.Count;
  for I := 0 to ACount - 1 do
    FPermissions.Remove(AProcessedPermissions[I]);
end;

procedure TdxDocRangeEditPermissionIterator.Read(AFib: TdxFileInformationBlock; AReader: TBinaryReader);
begin
  FProtectionInfos := TdxRangeEditPermissionsStringTable.FromStream(AReader, AFib.RangeEditPermissionsInformationOffset, AFib.RangeEditPermissionsInformationSize);
  FirstTable := TdxDocBookmarkFirstTable.FromStream(AReader, AFib.RangeEditPermissionsStartInfoOffset, AFib.RangeEditPermissionsStartInfoSize, BookmarkIndexSize);
  LimTable := TdxDocBookmarkLimTable.FromStream(AReader, AFib.RangeEditPermissionsEndInfoOffset, AFib.RangeEditPermissionsEndInfoSize);
  FUsers := TdxUserProtectionStringTable.FromStream(AReader, AFib.RangeEditPermissionsUsernamesOffset, AFib.RangeEditPermissionsUsernamesSize);
  InitRangeEditPermissions;
  InitConverter;
end;

procedure TdxDocRangeEditPermissionIterator.InitRangeEditPermissions;
var
  ACount, I, AStart, AEnd: Integer;
  AGrantedUsers: TdxStringList;
  APermissions: TdxObjectList<TdxDocImportRangePermissionInfo>;
begin
  ACount := FProtectionInfos.Count;

  FPermissions.Free;
  FPermissions := TdxObjectList<TdxDocImportRangePermissionInfo>.Create;

  for I := 0 to ACount - 1 do
  begin
    if FirstTable.BookmarkFirstDescriptors[I].Column then
      Continue;
    AGrantedUsers := FUsers.GetUsersById(FProtectionInfos.ProtectionInfos[I].Uid);
    AStart := FirstTable.CharacterPositions[I];
    AEnd := LimTable.CharacterPositions[I];
    APermissions := CreatePermissionInfos(AStart, AEnd, FProtectionInfos.ProtectionInfos[I].ProtectionType, AGrantedUsers);
    try
      FPermissions.AddRange(APermissions);
      APermissions.OwnsObjects := False;
    finally
      APermissions.Free;
    end;
  end;
end;

function TdxDocRangeEditPermissionIterator.CreatePermissionInfos(AStart: Integer; AEnd: Integer;
  AType: TdxDocumentProtectionType; AUsers: TdxStringList): TdxObjectList<TdxDocImportRangePermissionInfo>;
var
  AInfo: TdxDocImportRangePermissionInfo;
  ACount, I: Integer;
begin
  Result := TdxObjectList<TdxDocImportRangePermissionInfo>.Create;
  if AUsers = nil then
  begin
    AInfo := TdxDocImportRangePermissionInfo.Create(AStart, AEnd);
    AInfo.PermissionInfo.Group := DefaultGroupName;
    AInfo.ProtectionType := AType;
    Result.Add(AInfo);
    Exit;
  end;
  ACount := AUsers.Count;
  for I := 0 to ACount - 1 do
  begin
    AInfo := TdxDocImportRangePermissionInfo.Create(AStart, AEnd);
    AInfo.PermissionInfo.UserName := AUsers[I];
    AInfo.ProtectionType := AType;
    Result.Add(AInfo);
  end;
end;

procedure TdxDocRangeEditPermissionIterator.Write(AFib: TdxFileInformationBlock; AWriter: TBinaryWriter);
begin
  AFib.RangeEditPermissionsInformationOffset := AWriter.BaseStream.Position;
  FProtectionInfos.Write(AWriter);
  AFib.RangeEditPermissionsInformationSize := AWriter.BaseStream.Position - AFib.RangeEditPermissionsInformationOffset;
  AFib.RangeEditPermissionsStartInfoOffset := AWriter.BaseStream.Position;
  FirstTable.Write(AWriter, BookmarkIndexSize);
  AFib.RangeEditPermissionsStartInfoSize := AWriter.BaseStream.Position - AFib.RangeEditPermissionsStartInfoOffset;
  AFib.RangeEditPermissionsEndInfoOffset := AWriter.BaseStream.Position;
  LimTable.Write(AWriter);
  AFib.RangeEditPermissionsEndInfoSize := AWriter.BaseStream.Position - AFib.RangeEditPermissionsEndInfoOffset;
  AFib.RangeEditPermissionsUsernamesOffset := AWriter.BaseStream.Position;
  FUsers.Write(AWriter);
  AFib.RangeEditPermissionsUsernamesSize := AWriter.BaseStream.Position - AFib.RangeEditPermissionsUsernamesOffset;
end;

procedure TdxDocRangeEditPermissionIterator.AddPermissionStart(APermission: TdxRangePermission; AStartPosition: Integer);
var
  AId: Integer;
begin
  AId := APermission.GetHashCode;
  if not FPermissionStartPositions.ContainsKey(AId) then
    FPermissionStartPositions.Add(AId, AStartPosition);
end;

procedure TdxDocRangeEditPermissionIterator.AddPermissionEnd(APermission: TdxRangePermission; AEndPosition: Integer);
var
  AId, AStartPosition, AUserId: Integer;
begin
  AId := APermission.GetHashCode;
  if not FPermissionStartPositions.TryGetValue(AId, AStartPosition) then
    Exit;
  AUserId := FUsers.GetUserIdByName(APermission.UserName);
  FProtectionInfos.ProtectionInfos.Add(TdxDocProtectionInfo.Create(AUserId, TdxDocumentProtectionType.None));
  FirstTable.AddEntry(AStartPosition, TdxDocBookmarkFirstDescriptor.Create(CurrentIndex));
  LimTable.CharacterPositions.Add(AEndPosition);
  CurrentIndex := CurrentIndex + 1;
end;

end.
