unit EngineAPI;

interface

uses
  SysUtils, Windows, EngineType;

const
  BUFFERSIZE = 10000;

procedure TMemoryManager_Get(MemManager: _LPTMEMORYMANAGE); stdcall;

function TMemoryManager_AllocMemCount(): Integer; stdcall;

function TMemoryManager_AllocMemSize(): Integer; stdcall;

function TList_Create(): TList; stdcall;

procedure TList_Free(List: TList); stdcall;

function TList_Count(List: TList): Integer; stdcall;

function TList_Add(List: TList; Item: Pointer): Integer; stdcall;

procedure TList_Insert(List: TList; nIndex: Integer; Item: Pointer); stdcall;

function TList_Get(List: TList; nIndex: Integer): Pointer; stdcall;

procedure TList_Put(List: TList; nIndex: Integer; Item: Pointer); stdcall;

procedure TList_Delete(List: TList; nIndex: Integer); stdcall;

procedure TList_Clear(List: TList); stdcall;

procedure TList_Exchange(List: TList; nIndex1, nIndex2: Integer); stdcall;

function TStringList_Create(): TStringList; stdcall;

procedure TStringList_Free(List: TStringList); stdcall;

function TStringList_Count(List: TStringList): Integer; stdcall;

function TStringList_Add(List: TStringList; S: PAnsiChar): Integer; stdcall;

function TStringList_AddObject(List: TStringList; S: PAnsiChar; AObject: TObject): Integer; stdcall;

procedure TStringList_Insert(List: TStringList; nIndex: Integer; S: PAnsiChar); stdcall;

function TStringList_Get(List: TStringList; nIndex: Integer): PAnsiChar; stdcall;

function TStringList_GetObject(List: TStringList; nIndex: Integer): TObject; stdcall;

procedure TStringList_Put(List: TStringList; nIndex: Integer; S: PAnsiChar); stdcall;

procedure TStringList_PutObject(List: TStringList; nIndex: Integer; AObject: TObject); stdcall;

procedure TStringList_Delete(List: TStringList; nIndex: Integer); stdcall;

procedure TStringList_Clear(List: TStringList); stdcall;

procedure TStringList_Exchange(List: TStringList; nIndex1, nIndex2: Integer); stdcall;

procedure TStringList_LoadFormFile(List: TStringList; pszFileName: PAnsiChar); stdcall;

procedure TStringList_SaveToFile(List: TStringList; pszFileName: PAnsiChar); stdcall;

procedure MainOutMessage(pszMsg: PAnsiChar); stdcall;

procedure AddGameDataLog(pszMsg: PAnsiChar); stdcall;

function GetGameGoldName(): _LPTSHORTSTRING; stdcall;

function GetGameLogGold(): PBoolean; stdcall;

function GetGameLogGameGold(): PBoolean; stdcall;

function GetGameLogGamePoint(): PBoolean; stdcall;

function GetSellOffGoldTaxRate(): PInteger; stdcall;

function GetSellOffGameGoldTaxRate(): PInteger; stdcall;

function GetLevelItemRate(): PInteger; stdcall;

function GetLevelItemGoldCount(): PInteger; stdcall;

function GetLevelItemGameGoldCount(): PInteger; stdcall;

function GetLevelItemGameCount(): _LPTDIRNAME; stdcall;

procedure EDcode_Decode6BitBuf(pszSource: PAnsiChar; pszDest: PAnsiChar; nSrcLen, nDestLen: Integer); stdcall;

procedure EDcode_Encode6BitBuf(pszSource: PAnsiChar; pszDest: PAnsiChar; nSrcLen, nDestLen: Integer); stdcall;

procedure EDcode_SetDecode(Decode: _TEDCODE); stdcall;

procedure EDcode_SetEncode(Encode: _TEDCODE); stdcall;

procedure EDcode_DeCodeString(pszSource: PAnsiChar; var pszDest: array of AnsiChar); stdcall;

procedure EDcode_EncodeString(pszSource: PAnsiChar; var pszDest: array of AnsiChar); stdcall;

procedure EDcode_EncodeBuffer(Buf: PAnsiChar; bufsize: Integer; var pszDest: array of AnsiChar); stdcall;

procedure EDcode_DecodeBuffer(pszSource: PAnsiChar; pszDest: PAnsiChar; bufsize: Integer); stdcall;

function TConfig_sEnvirDir(): _LPTDIRNAME; stdcall;

function TConfig_sUserDataDir(): _LPTDIRNAME; stdcall;

function TBaseObject_Create(): TBaseObject; stdcall;

procedure TBaseObject_Free(BaseObject: TBaseObject); stdcall;

function TBaseObject_sMapFileName(BaseObject: TBaseObject): _LPTSHORTSTRING; stdcall;

function TBaseObject_sMapName(BaseObject: TBaseObject): _LPTSHORTSTRING; stdcall;

function TBaseObject_sMapNameA(BaseObject: TBaseObject): _LPTMAPNAME; stdcall;

function TBaseObject_sCharName(BaseObject: TBaseObject): _LPTSHORTSTRING; stdcall;

function TBaseObject_sCharNameA(BaseObject: TBaseObject): _LPTACTORNAME; stdcall;

function TBaseObject_nCurrX(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_nCurrY(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_btDirection(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_btGender(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_btHair(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_btJob(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_nGold(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_Ability(BaseObject: TBaseObject): _LPTABILITY; stdcall;

function TBaseObject_WAbility(BaseObject: TBaseObject): _LPTABILITY; stdcall;

function TBaseObject_nCharStatus(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_sHomeMap(BaseObject: TBaseObject): _LPTSHORTSTRING; stdcall;

function TBaseObject_nHomeX(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_nHomeY(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_boOnHorse(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_btHorseType(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_btDressEffType(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_nPkPoint(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_boAllowGroup(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_boAllowGuild(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_nFightZoneDieCount(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_nBonusPoint(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_nHungerStatus(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_boAllowGuildReCall(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_duBodyLuck(BaseObject: TBaseObject): PDouble; stdcall;

function TBaseObject_nBodyLuckLevel(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_wGroupRcallTime(BaseObject: TBaseObject): PWord; stdcall;

function TBaseObject_boAllowGroupReCall(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_nCharStatusEx(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_dwFightExp(BaseObject: TBaseObject): PLongWord; stdcall;

function TBaseObject_nViewRange(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_wAppr(BaseObject: TBaseObject): PWord; stdcall;

function TBaseObject_btRaceServer(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_btRaceImg(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_btHitPoint(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_nHitPlus(BaseObject: TBaseObject): PShortInt; stdcall;

function TBaseObject_nHitDouble(BaseObject: TBaseObject): PShortInt; stdcall;

function TBaseObject_boRecallSuite(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_nHealthRecover(BaseObject: TBaseObject): PShortInt; stdcall;

function TBaseObject_nSpellRecover(BaseObject: TBaseObject): PShortInt; stdcall;

function TBaseObject_btAntiPoison(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_nPoisonRecover(BaseObject: TBaseObject): PShortInt; stdcall;

function TBaseObject_nAntiMagic(BaseObject: TBaseObject): PShortInt; stdcall;

function TBaseObject_nLuck(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_nPerHealth(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_nPerHealing(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_nPerSpell(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_btGreenPoisoningPoint(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_nGoldMax(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_btSpeedPoint(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_btPermission(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_nHitSpeed(BaseObject: TBaseObject): PShortInt; stdcall;

function TBaseObject_TargetCret(BaseObject: TBaseObject): PTBaseObject; stdcall;

function TBaseObject_LastHiter(BaseObject: TBaseObject): PTBaseObject; stdcall;

function TBaseObject_ExpHiter(BaseObject: TBaseObject): PTBaseObject; stdcall;

function TBaseObject_btLifeAttrib(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_GroupOwner(BaseObject: TBaseObject): TBaseObject; stdcall;

function TBaseObject_GroupMembersList(BaseObject: TBaseObject): TStringList; stdcall;

function TBaseObject_boHearWhisper(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_boBanShout(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_boBanGuildChat(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_boAllowDeal(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_Master(BaseObject: TBaseObject): PTBaseObject; stdcall;

function TBaseObject_btAttatckMode(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_nNameColor(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_nLight(BaseObject: TBaseObject): PInteger; stdcall;

function TBaseObject_ItemList(BaseObject: TBaseObject): TList; stdcall;

function TBaseObject_MagicList(BaseObject: TBaseObject): TList; stdcall;

function TBaseObject_MyGuild(BaseObject: TBaseObject): TGuild; stdcall;

function TBaseObject_UseItems(BaseObject: TBaseObject): _LPTPLAYUSEITEMS; stdcall;

function TBaseObject_btMonsterWeapon(BaseObject: TBaseObject): PByte; stdcall;

function TBaseObject_PEnvir(BaseObject: TBaseObject): PTEnvirnoment; stdcall;

function TBaseObject_boGhost(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_boDeath(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_boNoItem(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_WAbil_HP(BaseObject: TBaseObject): PBoolean; stdcall;

function TBaseObject_boHero(BaseObject: TBaseObject): Boolean; stdcall;

function TBaseObject_DeleteBagItem(BaseObject: TBaseObject; UserItem: _LPTUSERITEM): BOOL; stdcall;

function TBaseObject_AddCustomData(BaseObject: TBaseObject; Data: Pointer): Integer; stdcall;

function TBaseObject_GetCustomData(BaseObject: TBaseObject; nIndex: Integer): Pointer; stdcall;

procedure TBaseObject_SendMsg(SelfObject, BaseObject: TBaseObject; wIdent, wParam: Word; nParam1, nParam2, nParam3: Integer; pszMsg: PAnsiChar); stdcall;

procedure TBaseObject_SendRefMsg(BaseObject: TBaseObject; wIdent, wParam: Word; nParam1, nParam2, nParam3: Integer; pszMsg: PAnsiChar); stdcall;

procedure TBaseObject_SysMsg(BaseObject: TBaseObject; pszMsg: PAnsiChar; MsgColor: _TMSGCOLOR; MsgType: _TMSGTYPE); stdcall;

function TBaseObject_GetFrontPosition(BaseObject: TBaseObject; var nX: Integer; var nY: Integer): Boolean; stdcall;

function TBaseObject_GetRecallXY(BaseObject: TBaseObject; nX, nY: Integer; nRange: Integer; var nDX: Integer; var nDY: Integer): Boolean; stdcall;

procedure TBaseObject_SpaceMove(BaseObject: TBaseObject; pszMap: PAnsiChar; nX, nY: Integer; nInt: Integer); stdcall;

procedure TBaseObject_FeatureChanged(BaseObject: TBaseObject); stdcall;

procedure TBaseObject_StatusChanged(BaseObject: TBaseObject); stdcall;

function TBaseObject_GetFeatureToLong(BaseObject: TBaseObject): Integer; stdcall;

function TBaseObject_GetFeature(SelfObject, BaseObject: TBaseObject): Integer; stdcall;

function TBaseObject_GetCharColor(SelfObject, BaseObject: TBaseObject): Byte; stdcall;

function TBaseObject_GetNamecolor(BaseObject: TBaseObject): Byte; stdcall;

procedure TBaseObject_GoldChanged(BaseObject: TBaseObject); stdcall;

procedure TBaseObject_GameGoldChanged(BaseObject: TBaseObject); stdcall;

function TBaseObject_MagCanHitTarget(BaseObject: TBaseObject; nX, nY: Integer; TargeTBaseObject: TBaseObject): Boolean; stdcall;

function TBaseObject_IsProtectTarget(AObject, BObject: TBaseObject): Boolean; stdcall;

function TBaseObject_IsAttackTarget(AObject, BObject: TBaseObject): Boolean; stdcall;

function TBaseObject_IsProperTarget(AObject, BObject: TBaseObject): Boolean; stdcall;

function TBaseObject_IsProperFriend(AObject, BObject: TBaseObject): Boolean; stdcall;

procedure TBaseObject_TrainSkillPoint(BaseObject: TBaseObject; UserMagic: _LPTUSERMAGIC; nTranPoint: Integer); stdcall;

function TBaseObject_GetAttackPower(BaseObject: TBaseObject; nBasePower, nPower: Integer): Integer; stdcall;

function TBaseObject_MakeSlave(BaseObject: TBaseObject; pszMonName: PAnsiChar; nMakeLevel, nExpLevel, nMaxMob, nType: Integer; dwRoyaltySec: LongWord): TBaseObject; stdcall;

procedure TBaseObject_MakeGhost(BaseObject: TBaseObject); stdcall;

procedure TBaseObject_RefNameColor(BaseObject: TBaseObject); stdcall;
  //AddItem 占用内存由自己处理，API内部会自动申请内存

function TBaseObject_AddItemToBag(BaseObject: TBaseObject; AddItem: _LPTUSERITEM): BOOL; stdcall;

function TBaseObject_AddItemToStorage(BaseObject: TBaseObject; AddItem: _LPTUSERITEM): BOOL; stdcall;

procedure TBaseObject_ClearBagItem(BaseObject: TBaseObject); stdcall;

procedure TBaseObject_ClearStorageItem(BaseObject: TBaseObject); stdcall;

procedure TBaseObject_SetHookGetFeature(ObjectActionFeature: _TOBJECTACTIONFEATURE); stdcall;

procedure TBaseObject_SetHookEnterAnotherMap(EnterAnotherMap: _TOBJECTACTIONENTERMAP); stdcall;

procedure TBaseObject_SetHookObjectDie(ObjectDie: _TOBJECTACTIONEX); stdcall;

procedure TBaseObject_SetHookChangeCurrMap(ChangeCurrMap: _TOBJECTACTIONEX); stdcall;

function TBaseObject_GetPoseCreate(BaseObject: TBaseObject): TBaseObject; stdcall;

function TPlayObject_IsEnoughBag(PlayObject: TPlayObject): Boolean; stdcall;

function TPlayObject_nSoftVersionDate(PlayObject: TPlayObject): PInteger; stdcall;

function TPlayObject_nSoftVersionDateEx(PlayObject: TPlayObject): PInteger; stdcall;

function TPlayObject_dLogonTime(PlayObject: TPlayObject): PDateTime; stdcall;

function TPlayObject_dwLogonTick(PlayObject: TPlayObject): PLongWord; stdcall;

function TPlayObject_nMemberType(PlayObject: TPlayObject): PInteger; stdcall;

function TPlayObject_nMemberLevel(PlayObject: TPlayObject): PInteger; stdcall;

function TPlayObject_nGameGold(PlayObject: TPlayObject): PInteger; stdcall;

procedure TPlayObject_DecGameGold(PlayObject: TPlayObject; nDecGameGold: Integer); stdcall;

function TPlayObject_nGamePoint(PlayObject: TPlayObject): PInteger; stdcall;

function TPlayObject_nPayMentPoint(PlayObject: TPlayObject): PInteger; stdcall;

function TPlayObject_nClientFlag(PlayObject: TPlayObject): PInteger; stdcall;

function TPlayObject_nSelectID(PlayObject: TPlayObject): PInteger; stdcall;

function TPlayObject_nClientFlagMode(PlayObject: TPlayObject): PInteger; stdcall;

function TPlayObject_dwClientTick(PlayObject: TPlayObject): PLongWord; stdcall;

function TPlayObject_wClientType(PlayObject: TPlayObject): PWord; stdcall;

function TPlayObject_sBankPassword(PlayObject: TPlayObject): _LPTBANKPWD; stdcall;

function TPlayObject_nBankGold(PlayObject: TPlayObject): PInteger; stdcall;

function TPlayObject_Create(): TPlayObject; stdcall;

procedure TPlayObject_Free(PlayObject: TPlayObject); stdcall;

procedure TPlayObject_SendSocket(PlayObject: TPlayObject; DefMsg: _LPTDEFAULTMESSAGE; pszMsg: PAnsiChar); stdcall;

procedure TPlayObject_SendDefMessage(PlayObject: TPlayObject; wIdent: Word; nRecog: Integer; nParam, nTag, nSeries: Word; pszMsg: PAnsiChar); stdcall;

procedure TPlayObject_SendAddItem(PlayObject: TPlayObject; AddItem: _LPTUSERITEM); stdcall;

procedure TPlayObject_SendDelItem(PlayObject: TPlayObject; AddItem: _LPTUSERITEM); stdcall;

function TPlayObject_TargetInNearXY(PlayObject: TPlayObject; Target: TBaseObject; nX, nY: Integer): Boolean; stdcall;

procedure TPlayObject_SetBankPassword(PlayObject: TPlayObject; pszPassword: PAnsiChar); stdcall;

procedure TPlayObject_SetHookCreate(PlayObjectCreate: _TOBJECTACTION); stdcall;

function TPlayObject_GetHookCreate(): _TOBJECTACTION; stdcall;

procedure TPlayObject_SetHookDestroy(PlayObjectDestroy: _TOBJECTACTION); stdcall;

function TPlayObject_GetHookDestroy(): _TOBJECTACTION; stdcall;

procedure TPlayObject_SetHookUserLogin1(PlayObjectUserLogin: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookUserLogin2(PlayObjectUserLogin: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookUserLogin3(PlayObjectUserLogin: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookUserLogin4(PlayObjectUserLogin: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookUserCmd(PlayObjectUserCmd: _TOBJECTUSERCMD); stdcall;

function TPlayObject_GetHookUserCmd(): _TOBJECTUSERCMD; stdcall;

procedure TPlayObject_SetHookPlayOperateMessage(PlayObjectOperateMessage: _TOBJECTOPERATEMESSAGE); stdcall;

function TPlayObject_GetHookPlayOperateMessage(): _TOBJECTOPERATEMESSAGE; stdcall;

procedure TPlayObject_SetHookClientQueryBagItems(ClientQueryBagItems: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookClientQueryUserState(ClientQueryUserState: _TOBJECTACTIONXY); stdcall;

procedure TPlayObject_SetHookSendActionGood(SendActionGood: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookSendActionFail(SendActionFail: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookSendWalkMsg(ObjectActioinXYD: _TOBJECTACTIONXYD); stdcall;

procedure TPlayObject_SetHookSendHorseRunMsg(ObjectActioinXYD: _TOBJECTACTIONXYD); stdcall;

procedure TPlayObject_SetHookSendRunMsg(ObjectActioinXYD: _TOBJECTACTIONXYD); stdcall;

procedure TPlayObject_SetHookSendDeathMsg(ObjectActioinXYDM: _TOBJECTACTIONXYDM); stdcall;

procedure TPlayObject_SetHookSendSkeletonMsg(ObjectActioinXYD: _TOBJECTACTIONXYD); stdcall;

procedure TPlayObject_SetHookSendAliveMsg(ObjectActioinXYD: _TOBJECTACTIONXYD); stdcall;

procedure TPlayObject_SetHookSendSpaceMoveMsg(ObjectActioinXYDWS: _TOBJECTACTIONXYDWS); stdcall;

procedure TPlayObject_SetHookSendChangeFaceMsg(ObjectActioinObject: _TOBJECTACTIONOBJECT); stdcall;

procedure TPlayObject_SetHookSendUseitemsMsg(ObjectActioin: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookSendUserLevelUpMsg(ObjectActioinObject: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookSendUserAbilieyMsg(ObjectActioinObject: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookSendUserStatusMsg(ObjectActioinXYDWS: _TOBJECTACTIONXYDWS); stdcall;

procedure TPlayObject_SetHookSendUserStruckMsg(ObjectActioinObject: _TOBJECTACTIONOBJECT); stdcall;

procedure TPlayObject_SetHookSendUseMagicMsg(ObjectActioin: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookSendSocket(SendSocket: _TPLAYSENDSOCKET); stdcall;

procedure TPlayObject_SetHookSendGoodsList(SendGoodsList: _TOBJECTACTIONSENDGOODS); stdcall;

procedure TPlayObject_SetCheckClientDropItem(ActionDropItem: _TOBJECTACTIONITEM); stdcall;

procedure TPlayObject_SetCheckClientDealItem(ActionItem: _TOBJECTACTIONITEM); stdcall;

procedure TPlayObject_SetCheckClientStorageItem(ActionItem: _TOBJECTACTIONITEM); stdcall;

procedure TPlayObject_SetCheckClientRepairItem(ActionItem: _TOBJECTACTIONITEM); stdcall;

procedure TPlayObject_SetHookCheckUserItems(ObjectActioin: _TOBJECTACTIONCHECKUSEITEM); stdcall;

procedure TPlayObject_SetHookRun(PlayRun: _TOBJECTACTION); stdcall;

procedure TPlayObject_SetHookFilterMsg(FilterMsg: _TOBJECTFILTERMSG); stdcall;

function TPlayObject_GetPlayObjectTick(PlayObject: TPlayObject; nCount: Integer): PLongWord; stdcall;

procedure TPlayObject_SetPlayObjectTick(PlayObject: TPlayObject; nCount: Integer); stdcall;

function TPlayObject_CheckPlaySideNpc(PlayObject: TPlayObject; NormNpc: TNormNpc): TNormNpc; stdcall;

function TNormNpc_sFilePath(NormNpc: TNormNpc): _LPTPATHNAME; stdcall;

function TNormNpc_sPath(NormNpc: TNormNpc): _LPTPATHNAME; stdcall;

procedure TNormNpc_GetLineVariableText(NormNpc: TNormNpc; BaseObject: TBaseObject; pszMsg: PAnsiChar; pszOutMsg: PAnsiChar; nOutMsgLen: Integer); stdcall;

procedure TNormNpc_SetScriptActionCmd(ActionCmd: _TSCRIPTCMD); stdcall;

function TNormNpc_GetScriptActionCmd(): _TSCRIPTCMD; stdcall;

procedure TNormNpc_SetScriptConditionCmd(ConditionCmd: _TSCRIPTCMD); stdcall;

function TNormNpc_GetScriptConditionCmd(): _TSCRIPTCMD; stdcall;

function TNormNpc_GetManageNpc(): TNormNpc; stdcall;

function TNormNpc_GetFunctionNpc(): TNormNpc; stdcall;

procedure TNormNpc_GotoLable(NormNpc: TNormNpc; PlayObject: TPlayObject; pszLabel: PAnsiChar); stdcall;

procedure TNormNpc_SetScriptAction(ScriptAction: _TSCRIPTACTION); stdcall;

function TNormNpc_GetScriptAction(): _TSCRIPTACTION; stdcall;

procedure TNormNpc_SetScriptCondition(ScriptAction: _TSCRIPTCONDITION); stdcall;

function TNormNpc_GetScriptCondition(): _TSCRIPTCONDITION; stdcall;

function TMerchant_GoodsList(Merchant: TMerchant): TList; stdcall;

function TMerchant_GetItemPrice(Merchant: TMerchant; nIndex: Integer): Integer; stdcall;

function TMerchant_GetUserPrice(Merchant: TMerchant; PlayObject: TPlayObject; nPrice: Integer): Integer; stdcall;

function TMerchant_GetUserItemPrice(Merchant: TMerchant; UserItem: _LPTUSERITEM): Integer; stdcall;

procedure TMerchant_SetHookClientGetDetailGoodsList(GetDetailGoods: _TOBJECTACTIONDETAILGOODS); stdcall;

function TMerchant_boSellOff(Merchant: TMerchant): PBoolean; stdcall;

function TMapManager_FindMap(pszMapName: PAnsiChar): TEnvirnoment; stdcall;

function TEnvirnoment_GetRangeBaseObject(Envir: TEnvirnoment; nX, nY, nRage: Integer; boFlag: Boolean; BaseObjectList: TList): Integer; stdcall;

function TEnvirnoment_boCANRIDE(Envir: TEnvirnoment): PBoolean; stdcall;

function TEnvirnoment_boCANBAT(Envir: TEnvirnoment): PBoolean; stdcall;

function TUserEngine_Create(): TUserEngine; stdcall;

procedure TUserEngine_Free(UserEngine: TUserEngine); stdcall;

function TUserEngine_GetUserEngine(): TUserEngine; stdcall;

function TUserEngine_GetPlayObject(szPlayName: PAnsiChar; boGetHide: Boolean): TPlayObject; stdcall;

function TUserEngine_GetLoadPlayList(): TStringList; stdcall;

function TUserEngine_GetPlayObjectList(): TStringList; stdcall;

function TUserEngine_GetLoadPlayCount(): Integer; stdcall;

function TUserEngine_GetPlayObjectCount(): Integer; stdcall;

function TUserEngine_GetStdItemByName(pszItemName: PAnsiChar): _LPTSTDITEM; stdcall;

function TUserEngine_GetStdItemByIndex(nIndex: Integer): _LPTSTDITEM; stdcall;

function TUserEngine_CopyToUserItemFromName(const pszItemName: PAnsiChar; UserItem: _LPTUSERITEM): BOOL; stdcall;

function TUserEngine_GetStdItemList(): TList; stdcall;

function TUserEngine_GetClientItem(UserItem: _LPTUSERITEM; var ClientItem: _TCLIENTITEM): Boolean; stdcall;

procedure TUserEngine_SetHookRun(UserEngineRun: _TOBJECTACTION); stdcall;

function TUserEngine_GetHookRun(): _TOBJECTACTION; stdcall;

procedure TUserEngine_SetHookClientUserMessage(ClientMsg: _TOBJECTCLIENTMSG); stdcall;

function TUserEngine_GetMapMonster(): _TOBJECTACTION; stdcall;

function M2Share_GetNoClearMonList(): _TOBJECTACTION; stdcall;

function TGuild_RankList(Guild: TGuild): TList; stdcall;

function TItemUnit_GetItem(nIndex: Integer): TItem; stdcall;

function TItemUnit_sItemName(Item: TItem): _LPTSHORTSTRING; stdcall;

function TItemUnit_nStdMode(Item: TItem): PByte; stdcall;

function TItemUnit_nSellOff(Item: TItem): PByte; stdcall;

function TItemUnit_nLevelItem(Item: TItem): PByte; stdcall;

function TItemUnit_nNeedIdentify(Item: TItem): PByte; stdcall;

function TItemUnit_nItemType(Item: TItem): PByte; stdcall;

procedure TItemUnit_GetStandardItem(Item: TItem; var StdItem: _TSTDITEM); stdcall;

procedure TItemUnit_GetItemAddValue(UserItem: _LPTUSERITEM; var StdItem: _TSTDITEM); stdcall;

function TMagicManager_GetMagicManager(): TMagicManager; stdcall;

function TMagicManager_DoSpell(MagicManager: TMagicManager; PlayObject: TPlayObject; UserMagic: _LPTUSERMAGIC; nTargetX, nTargetY: Integer; TargeTBaseObject: TBaseObject): Boolean; stdcall;

function TMagicManager_MPow(UserMagic: _LPTUSERMAGIC): Integer; stdcall;

function TMagicManager_GetPower(nPower: Integer; UserMagic: _LPTUSERMAGIC): Integer; stdcall;

function TMagicManager_GetPower13(nInt: Integer; UserMagic: _LPTUSERMAGIC): Integer; stdcall;

function TMagicManager_GetRPow(wInt: Integer): Word; stdcall;

function TMagicManager_IsWarrSkill(MagicManager: TMagicManager; wMagIdx: Integer): Boolean; stdcall;

function TMagicManager_MagBigHealing(MagicManager: TMagicManager; PlayObject: TBaseObject; nPower, nX, nY: integer): Boolean; stdcall;

function TMagicManager_MagPushArround(MagicManager: TMagicManager; PlayObject: TBaseObject; nPushLevel: integer): integer; stdcall;

function TMagicManager_MagPushArroundTaos(MagicManager: TMagicManager; PlayObject: TBaseObject; nPushLevel: integer): integer; stdcall;

function TMagicManager_MagTurnUndead(MagicManager: TMagicManager; BaseObject, TargeTBaseObject: TBaseObject; nTargetX, nTargetY: Integer; nLevel: Integer): Boolean; stdcall;

function TMagicManager_MagMakeHolyCurtain(MagicManager: TMagicManager; BaseObject: TBaseObject; nPower: Integer; nX, nY: Integer): Integer; stdcall;

function TMagicManager_MagMakeGroupTransparent(MagicManager: TMagicManager; BaseObject: TBaseObject; nX, nY: Integer; nHTime: Integer): Boolean; stdcall;

function TMagicManager_MagTamming(MagicManager: TMagicManager; BaseObject, TargeTBaseObject: TBaseObject; nTargetX, nTargetY: Integer; nMagicLevel: Integer): Boolean; stdcall;

function TMagicManager_MagSaceMove(MagicManager: TMagicManager; BaseObject: TBaseObject; nLevel: integer): Boolean; stdcall;

function TMagicManager_MagMakeFireCross(MagicManager: TMagicManager; PlayObject: TPlayObject; nDamage, nHTime, nX, nY: Integer): Integer; stdcall;

function TMagicManager_MagBigExplosion(MagicManager: TMagicManager; BaseObject: TBaseObject; nPower, nX, nY: Integer; nRage: Integer): Boolean; stdcall;

function TMagicManager_MagElecBlizzard(MagicManager: TMagicManager; BaseObject: TBaseObject; nPower: integer): Boolean; stdcall;

function TMagicManager_MabMabe(MagicManager: TMagicManager; BaseObject, TargeTBaseObject: TBaseObject; nPower, nLevel, nTargetX, nTargetY: Integer): Boolean; stdcall;

function TMagicManager_MagMakeSlave(MagicManager: TMagicManager; PlayObject: TPlayObject; UserMagic: _LPTUSERMAGIC): Boolean; stdcall;

function TMagicManager_MagMakeSinSuSlave(MagicManager: TMagicManager; PlayObject: TPlayObject; UserMagic: _LPTUSERMAGIC): Boolean; stdcall;

function TMagicManager_MagWindTebo(MagicManager: TMagicManager; PlayObject: TPlayObject; UserMagic: _LPTUSERMAGIC): Boolean; stdcall;

function TMagicManager_MagGroupLightening(MagicManager: TMagicManager; PlayObject: TPlayObject; UserMagic: _LPTUSERMAGIC; nTargetX, nTargetY: Integer; TargeTBaseObject: TBaseObject; var boSpellFire: Boolean): Boolean; stdcall;

function TMagicManager_MagGroupAmyounsul(MagicManager: TMagicManager; PlayObject: TPlayObject; UserMagic: _LPTUSERMAGIC; nTargetX, nTargetY: Integer; TargeTBaseObject: TBaseObject): Boolean; stdcall;

function TMagicManager_MagGroupDeDing(MagicManager: TMagicManager; PlayObject: TPlayObject; UserMagic: _LPTUSERMAGIC; nTargetX, nTargetY: Integer; TargeTBaseObject: TBaseObject): Boolean; stdcall;

function TMagicManager_MagGroupMb(MagicManager: TMagicManager; PlayObject: TPlayObject; UserMagic: _LPTUSERMAGIC; nTargetX, nTargetY: Integer; TargeTBaseObject: TBaseObject): Boolean; stdcall;

function TMagicManager_MagHbFireBall(MagicManager: TMagicManager; PlayObject: TPlayObject; UserMagic: _LPTUSERMAGIC; nTargetX, nTargetY: Integer; var TargeTBaseObject: TBaseObject): Boolean; stdcall;

function TMagicManager_MagLightening(MagicManager: TMagicManager; PlayObject: TPlayObject; UserMagic: _LPTUSERMAGIC; nTargetX, nTargetY: Integer; var TargeTBaseObject: TBaseObject): Boolean; stdcall;

procedure TMagicManager_SetHookDoSpell(doSpell: _TDOSPELL); stdcall;

procedure TRunSocket_CloseUser(GateIdx, nSocket: Integer); stdcall;

procedure TRunSocket_SetHookExecGateMsgOpen(RunSocketExecGateMsg: TRunSocketObject_Open); stdcall;

procedure TRunSocket_SetHookExecGateMsgClose(RunSocketExecGateMsg: TRunSocketObject_Close); stdcall;

procedure TRunSocket_SetHookExecGateMsgEeceiveOK(RunSocketExecGateMsg: TRunSocketObject_SetHookExecGateMsgEeceiveOK); stdcall;

procedure TRunSocket_SetHookExecGateMsgData(RunSocketExecGateMsg: TRunSocketObject_Data); stdcall;

procedure ShortStringToPChar(S: _LPTSHORTSTRING; pszDest: PAnsiChar);

function EncodeMessage(smsg: _TDEFAULTMESSAGE): string;

function DecodeMessage(str: string): _TDEFAULTMESSAGE;

function EncodeString(str: string): string;

function DecodeString(str: string): string;

function EncodeBuffer(buf: pAnsiChar; bufsize: integer): string;

procedure DecodeBuffer(src: string; buf: PAnsiChar; bufsize: integer);

function MakeDefaultMsg(wIdent: Word; nRecog: Integer; wParam, wTag, wSeries: Word): _TDEFAULTMESSAGE;

implementation

procedure TMemoryManager_Get; external LibName name 'TMemoryManager_Get';

function TMemoryManager_AllocMemCount; external LibName name 'TMemoryManager_AllocMemCount';

function TMemoryManager_AllocMemSize; external LibName name 'TMemoryManager_AllocMemSize';

function TList_Create; external LibName name 'TList_Create';

procedure TList_Free; external LibName name 'TList_Free';

function TList_Count; external LibName name 'TList_Count';

function TList_Add; external LibName name 'TList_Add';

procedure TList_Insert; external LibName name 'TList_Insert';

function TList_Get; external LibName name 'TList_Get';

procedure TList_Put; external LibName name 'TList_Put';

procedure TList_Delete; external LibName name 'TList_Delete';

procedure TList_Clear; external LibName name 'TList_Delete';

procedure TList_Exchange; external LibName name 'TList_Delete';

function TStringList_Create; external LibName name 'TStringList_Create';

procedure TStringList_Free; external LibName name 'TStringList_Free';

function TStringList_Count; external LibName name 'TStringList_Count';

function TStringList_Add; external LibName name 'TStringList_Add';

function TStringList_AddObject; external LibName name 'TStringList_AddObject';

procedure TStringList_Insert; external LibName name 'TStringList_Insert';

function TStringList_Get; external LibName name 'TStringList_Get';

function TStringList_GetObject; external LibName name 'TStringList_GetObject';

procedure TStringList_Put; external LibName name 'TStringList_Put';

procedure TStringList_PutObject; external LibName name 'TStringList_PutObject';

procedure TStringList_Delete; external LibName name 'TStringList_Delete';

procedure TStringList_Clear; external LibName name 'TStringList_Clear';

procedure TStringList_Exchange; external LibName name 'TStringList_Exchange';

procedure TStringList_LoadFormFile; external LibName name 'TStringList_Exchange';

procedure TStringList_SaveToFile; external LibName name 'TStringList_Exchange';

procedure MainOutMessage; external LibName name 'MainOutMessageAPI';

procedure AddGameDataLog; external LibName name 'AddGameDataLogAPI';

function GetGameGoldName; external LibName name 'GetGameGoldName';

function GetGameLogGold; external LibName name 'GetGameLogGold';

function GetGameLogGameGold; external LibName name 'GetGameLogGameGold';

function GetGameLogGamePoint; external LibName name 'GetGameLogGamePoint';

function GetSellOffGoldTaxRate; external LibName name 'GetSellOffGoldTaxRate';

function GetSellOffGameGoldTaxRate; external LibName name 'GetSellOffGameGoldTaxRate';

function GetLevelItemRate; external LibName name 'GetLevelItemRate';

function GetLevelItemGoldCount; external LibName name 'GetLevelItemGoldCount';

function GetLevelItemGameGoldCount; external LibName name 'GetLevelItemGameGoldCount';

function GetLevelItemGameCount; external LibName name 'GetLevelItemGameCount';

procedure EDcode_Decode6BitBuf; external LibName name 'EDcode_Decode6BitBuf';

procedure EDcode_Encode6BitBuf; external LibName name 'EDcode_Encode6BitBuf';

procedure EDcode_SetDecode; external LibName name 'EDcode_SetDecode';

procedure EDcode_SetEncode; external LibName name 'EDcode_SetEncode';

procedure EDcode_EnCodeString; external LibName name 'EDcode_EnCodeString';

procedure EDcode_DeCodeString; external LibName name 'EDcode_DeCodeString';

procedure EDcode_EncodeBuffer; external LibName name 'EDcode_EncodeBuffer';

procedure EDcode_DecodeBuffer; external LibName name 'EDcode_DecodeBuffer';

function TConfig_sEnvirDir; external LibName name 'TConfig_sEnvirDir';

function TConfig_sUserDataDir; external LibName name 'Config_sUserDataDir';

function TBaseObject_Create; external LibName name 'TBaseObject_Create';

procedure TBaseObject_Free; external LibName name 'TBaseObject_Free';

function TBaseObject_sMapFileName; external LibName name 'TBaseObject_sMapFileName';

function TBaseObject_sMapName; external LibName name 'TBaseObject_sMapName';

function TBaseObject_sMapNameA; external LibName name 'TBaseObject_sMapNameA';

function TBaseObject_sCharName; external LibName name 'TBaseObject_sCharName';

function TBaseObject_sCharNameA; external LibName name 'TBaseObject_sCharNameA';

function TBaseObject_nCurrX; external LibName name 'TBaseObject_nCurrX';

function TBaseObject_nCurrY; external LibName name 'TBaseObject_nCurrY';

function TBaseObject_btDirection; external LibName name 'TBaseObject_btDirection';

function TBaseObject_btGender; external LibName name 'TBaseObject_btGender';

function TBaseObject_btHair; external LibName name 'TBaseObject_btHair';

function TBaseObject_btJob; external LibName name 'TBaseObject_btJob';

function TBaseObject_nGold; external LibName name 'TBaseObject_nGold';

function TBaseObject_Ability; external LibName name 'TBaseObject_Ability';

function TBaseObject_WAbility; external LibName name 'TBaseObject_WAbility';

function TBaseObject_nCharStatus; external LibName name 'TBaseObject_nCharStatus';

function TBaseObject_sHomeMap; external LibName name 'TBaseObject_sHomeMap';

function TBaseObject_nHomeX; external LibName name 'TBaseObject_nHomeX';

function TBaseObject_nHomeY; external LibName name 'TBaseObject_nHomeY';

function TBaseObject_boOnHorse; external LibName name 'TBaseObject_boOnHorse';

function TBaseObject_btHorseType; external LibName name 'TBaseObject_btHorseType';

function TBaseObject_btDressEffType; external LibName name 'TBaseObject_btDressEffType';

function TBaseObject_nPkPoint; external LibName name 'TBaseObject_nPkPoint';

function TBaseObject_boAllowGroup; external LibName name 'TBaseObject_boAllowGroup';

function TBaseObject_boAllowGuild; external LibName name 'TBaseObject_boAllowGuild';

function TBaseObject_nFightZoneDieCount; external LibName name 'TBaseObject_nFightZoneDieCount';

function TBaseObject_nBonusPoint; external LibName name 'TBaseObject_nBonusPoint';

function TBaseObject_nHungerStatus; external LibName name 'TBaseObject_nHungerStatus';

function TBaseObject_boAllowGuildReCall; external LibName name 'TBaseObject_boAllowGuildReCall';

function TBaseObject_duBodyLuck; external LibName name 'TBaseObject_duBodyLuck';

function TBaseObject_nBodyLuckLevel; external LibName name 'TBaseObject_nBodyLuckLevel';

function TBaseObject_wGroupRcallTime; external LibName name 'TBaseObject_wGroupRcallTime';

function TBaseObject_boAllowGroupReCall; external LibName name 'TBaseObject_boAllowGroupReCall';

function TBaseObject_nCharStatusEx; external LibName name 'TBaseObject_nCharStatusEx';

function TBaseObject_dwFightExp; external LibName name 'TBaseObject_dwFightExp';

function TBaseObject_nViewRange; external LibName name 'TBaseObject_nViewRange';

function TBaseObject_wAppr; external LibName name 'TBaseObject_wAppr';

function TBaseObject_btRaceServer; external LibName name 'TBaseObject_btRaceServer';

function TBaseObject_btRaceImg; external LibName name 'TBaseObject_btRaceImg';

function TBaseObject_btHitPoint; external LibName name 'TBaseObject_btHitPoint';

function TBaseObject_nHitPlus; external LibName name 'TBaseObject_nHitPlus';

function TBaseObject_nHitDouble; external LibName name 'TBaseObject_nHitDouble';

function TBaseObject_boRecallSuite; external LibName name 'TBaseObject_boRecallSuite';

function TBaseObject_nHealthRecover; external LibName name 'TBaseObject_nHealthRecover';

function TBaseObject_nSpellRecover; external LibName name 'TBaseObject_nSpellRecover';

function TBaseObject_btAntiPoison; external LibName name 'TBaseObject_btAntiPoison';

function TBaseObject_nPoisonRecover; external LibName name 'TBaseObject_nPoisonRecover';

function TBaseObject_nAntiMagic; external LibName name 'TBaseObject_nAntiMagic';

function TBaseObject_nLuck; external LibName name 'TBaseObject_nLuck';

function TBaseObject_nPerHealth; external LibName name 'TBaseObject_nPerHealth';

function TBaseObject_nPerHealing; external LibName name 'TBaseObject_nPerHealing';

function TBaseObject_nPerSpell; external LibName name 'TBaseObject_nPerSpell';

function TBaseObject_btGreenPoisoningPoint; external LibName name 'TBaseObject_btGreenPoisoningPoint';

function TBaseObject_nGoldMax; external LibName name 'TBaseObject_nGoldMax';

function TBaseObject_btSpeedPoint; external LibName name 'TBaseObject_btSpeedPoint';

function TBaseObject_btPermission; external LibName name 'TBaseObject_btPermission';

function TBaseObject_nHitSpeed; external LibName name 'TBaseObject_nHitSpeed';

function TBaseObject_TargetCret; external LibName name 'TBaseObject_TargetCret';

function TBaseObject_LastHiter; external LibName name 'TBaseObject_LastHiter';

function TBaseObject_ExpHiter; external LibName name 'TBaseObject_ExpHitter';

function TBaseObject_btLifeAttrib; external LibName name 'TBaseObject_btLifeAttrib';

function TBaseObject_GroupOwner; external LibName name 'TBaseObject_GroupOwner';

function TBaseObject_GroupMembersList; external LibName name 'TBaseObject_GroupMembersList';

function TBaseObject_boHearWhisper; external LibName name 'TBaseObject_boHearWhisper';

function TBaseObject_boBanShout; external LibName name 'TBaseObject_boBanShout';

function TBaseObject_boBanGuildChat; external LibName name 'TBaseObject_boBanGuildChat';

function TBaseObject_boAllowDeal; external LibName name 'TBaseObject_boAllowDeal';

function TBaseObject_Master; external LibName name 'TBaseObject_Master';

function TBaseObject_btAttatckMode; external LibName name 'TBaseObject_btAttatckMode';

function TBaseObject_nNameColor; external LibName name 'TBaseObject_nNameColor';

function TBaseObject_nLight; external LibName name 'TBaseObject_nLight';

function TBaseObject_ItemList; external LibName name 'TBaseObject_ItemList';

function TBaseObject_MagicList; external LibName name 'TBaseObject_MagicList';

function TBaseObject_MyGuild; external LibName name 'TBaseObject_MyGuild';

function TBaseObject_UseItems; external LibName name 'TBaseObject_UseItems';

function TBaseObject_btMonsterWeapon; external LibName name 'TBaseObject_btMonsterWeapon';

function TBaseObject_PEnvir; external LibName name 'TBaseObject_PEnvir';

function TBaseObject_boGhost; external LibName name 'TBaseObject_boGhost';

function TBaseObject_boDeath; external LibName name 'TBaseObject_boDeath';

function TBaseObject_boNoItem; external LibName name 'TBaseObject_boNoItem';

function TBaseObject_WAbil_HP; external LibName name 'TBaseObject_WAbil_HP';

function TBaseObject_boHero; external LibName name 'TBaseObject_boHero';

function TBaseObject_DeleteBagItem; external LibName name 'TBaseObject_DeleteBagItem';

function TBaseObject_AddCustomData; external LibName name 'TBaseObject_AddCustomData';

function TBaseObject_GetCustomData; external LibName name 'TBaseObject_GetCustomData';

procedure TBaseObject_SendMsg; external LibName name 'TBaseObject_SendMsg';

procedure TBaseObject_SendRefMsg; external LibName name 'TBaseObject_SendRefMsg';

procedure TBaseObject_SysMsg; external LibName name 'TBaseObject_SysMsg';

function TBaseObject_GetFrontPosition; external LibName name 'TBaseObject_GetFrontPosition';

function TBaseObject_GetRecallXY; external LibName name 'TBaseObject_GetRecallXY';

procedure TBaseObject_SpaceMove; external LibName name 'TBaseObject_SpaceMove';

procedure TBaseObject_FeatureChanged; external LibName name 'TBaseObject_FeatureChanged';

procedure TBaseObject_StatusChanged; external LibName name 'TBaseObject_StatusChanged';

function TBaseObject_GetFeatureToLong; external LibName name 'TBaseObject_GetFeatureToLong';

function TBaseObject_GetFeature; external LibName name 'TBaseObject_GetFeature';

function TBaseObject_GetCharColor; external LibName name 'TBaseObject_GetCharColor';

function TBaseObject_GetNamecolor; external LibName name 'TBaseObject_GetNamecolor';

procedure TBaseObject_GoldChanged; external LibName name 'TBaseObject_GoldChanged';

procedure TBaseObject_GameGoldChanged; external LibName name 'TBaseObject_GameGoldChanged';

function TBaseObject_MagCanHitTarget; external LibName name 'TBaseObject_MagCanHitTarget';

function TBaseObject_IsProtectTarget; external LibName name 'TBaseObject_IsProtectTarget';

function TBaseObject_IsAttackTarget; external LibName name 'TBaseObject_IsAttackTarget';

function TBaseObject_IsProperTarget; external LibName name 'TBaseObject_IsProperTarget';

function TBaseObject_IsProperFriend; external LibName name 'TBaseObject_IsProperFriend';

procedure TBaseObject_TrainSkillPoint; external LibName name 'TBaseObject_TrainSkillPoint';

function TBaseObject_GetAttackPower; external LibName name 'TBaseObject_GetAttackPower';

function TBaseObject_MakeSlave; external LibName name 'TBaseObject_MakeSlave';

procedure TBaseObject_MakeGhost; external LibName name 'TBaseObject_MakeGhost';

procedure TBaseObject_RefNameColor; external LibName name 'TBaseObject_RefNameColor';

function TBaseObject_AddItemToBag; external LibName name 'TBaseObject_AddItemToBag';

function TBaseObject_AddItemToStorage; external LibName name 'TBaseObject_AddItemToStorage';

procedure TBaseObject_ClearBagItem; external LibName name 'TBaseObject_ClearBagItem';

procedure TBaseObject_ClearStorageItem; external LibName name 'TBaseObject_ClearStorageItem';

procedure TBaseObject_SetHookGetFeature; external LibName name 'TBaseObject_SetHookGetFeature';

procedure TBaseObject_SetHookEnterAnotherMap; external LibName name 'TBaseObject_SetHookEnterAnotherMap';

procedure TBaseObject_SetHookObjectDie; external LibName name 'TBaseObject_SetHookObjectDie';

procedure TBaseObject_SetHookChangeCurrMap; external LibName name 'TBaseObject_SetHookChangeCurrMap';

function TBaseObject_GetPoseCreate; external LibName name 'TBaseObject_GetPoseCreate';

function TPlayObject_IsEnoughBag; external LibName name 'TPlayObject_IsEnoughBag';

function TPlayObject_nSoftVersionDate; external LibName name 'TPlayObject_nSoftVersionDate';

function TPlayObject_nSoftVersionDateEx; external LibName name 'TPlayObject_nSoftVersionDateEx';

function TPlayObject_dLogonTime; external LibName name 'TPlayObject_dLogonTime';

function TPlayObject_dwLogonTick; external LibName name 'TPlayObject_dwLogonTick';

function TPlayObject_nMemberType; external LibName name 'TPlayObject_nMemberType';

function TPlayObject_nMemberLevel; external LibName name 'TPlayObject_nMemberLevel';

function TPlayObject_nGameGold; external LibName name 'TPlayObject_nGameGold';

procedure TPlayObject_DecGameGold; external LibName name 'TPlayObject_DecGameGold';

function TPlayObject_nGamePoint; external LibName name 'TPlayObject_nGamePoint';

function TPlayObject_nPayMentPoint; external LibName name 'TPlayObject_nPayMentPoint';

function TPlayObject_nClientFlag; external LibName name 'TPlayObject_nClientFlag';

function TPlayObject_nSelectID; external LibName name 'TPlayObject_nSelectID';

function TPlayObject_nClientFlagMode; external LibName name 'TPlayObject_nClientFlagMode';

function TPlayObject_dwClientTick; external LibName name 'TPlayObject_dwClientTick';

function TPlayObject_wClientType; external LibName name 'TPlayObject_wClientType';

function TPlayObject_sBankPassword; external LibName name 'TPlayObject_sBankPassword';

function TPlayObject_nBankGold; external LibName name 'TPlayObject_nBankGold';

function TPlayObject_Create; external LibName name 'TPlayObject_Create';

procedure TPlayObject_Free; external LibName name 'TPlayObject_Free';

procedure TPlayObject_SendSocket; external LibName name 'TPlayObject_SendSocket';

procedure TPlayObject_SendDefMessage; external LibName name 'TPlayObject_SendDefMessage';

procedure TPlayObject_SendAddItem; external LibName name 'TPlayObject_SendAddItem';

procedure TPlayObject_SendDelItem; external LibName name 'TPlayObject_SendDelItem';

function TPlayObject_TargetInNearXY; external LibName name 'TPlayObject_TargetInNearXY';

procedure TPlayObject_SetBankPassword; external LibName name 'TPlayObject_SetBankPassword';

procedure TPlayObject_SetHookCreate; external LibName name 'TPlayObject_SetHookCreate';

function TPlayObject_GetHookCreate; external LibName name 'TPlayObject_GetHookCreate';

procedure TPlayObject_SetHookDestroy; external LibName name 'TPlayObject_SetHookDestroy';

function TPlayObject_GetHookDestroy; external LibName name 'TPlayObject_GetHookDestroy';

procedure TPlayObject_SetHookUserLogin1; external LibName name 'TPlayObject_SetHookUserLogin1';

procedure TPlayObject_SetHookUserLogin2; external LibName name 'TPlayObject_SetHookUserLogin2';

procedure TPlayObject_SetHookUserLogin3; external LibName name 'TPlayObject_SetHookUserLogin3';

procedure TPlayObject_SetHookUserLogin4; external LibName name 'TPlayObject_SetHookUserLogin4';

procedure TPlayObject_SetHookUserCmd; external LibName name 'TPlayObject_SetHookUserCmd';

function TPlayObject_GetHookUserCmd; external LibName name 'TPlayObject_GetHookUserCmd';

procedure TPlayObject_SetHookPlayOperateMessage; external LibName name 'TPlayObject_SetHookPlayOperateMessage';

function TPlayObject_GetHookPlayOperateMessage; external LibName name 'TPlayObject_GetHookPlayOperateMessage';

procedure TPlayObject_SetHookClientQueryBagItems; external LibName name 'TPlayObject_SetHookClientQueryBagItems';

procedure TPlayObject_SetHookClientQueryUserState; external LibName name 'TPlayObject_SetHookClientQueryUserState';

procedure TPlayObject_SetHookSendActionGood; external LibName name 'TPlayObject_SetHookSendActionGood';

procedure TPlayObject_SetHookSendActionFail; external LibName name 'TPlayObject_SetHookSendActionFail';

procedure TPlayObject_SetHookSendWalkMsg; external LibName name 'TPlayObject_SetHookSendWalkMsg';

procedure TPlayObject_SetHookSendHorseRunMsg; external LibName name 'TPlayObject_SetHookSendHorseRunMsg';

procedure TPlayObject_SetHookSendRunMsg; external LibName name 'TPlayObject_SetHookSendRunMsg';

procedure TPlayObject_SetHookSendDeathMsg; external LibName name 'TPlayObject_SetHookSendDeathMsg';

procedure TPlayObject_SetHookSendSkeletonMsg; external LibName name 'TPlayObject_SetHookSendSkeletonMsg';

procedure TPlayObject_SetHookSendAliveMsg; external LibName name 'TPlayObject_SetHookSendAliveMsg';

procedure TPlayObject_SetHookSendSpaceMoveMsg; external LibName name 'TPlayObject_SetHookSendSpaceMoveMsg';

procedure TPlayObject_SetHookSendChangeFaceMsg; external LibName name 'TPlayObject_SetHookSendChangeFaceMsg';

procedure TPlayObject_SetHookSendUseitemsMsg; external LibName name 'TPlayObject_SetHookSendUseitemsMsg';

procedure TPlayObject_SetHookSendUserLevelUpMsg; external LibName name 'TPlayObject_SetHookSendUserLevelUpMsg';

procedure TPlayObject_SetHookSendUserAbilieyMsg; external LibName name 'TPlayObject_SetHookSendUserAbilieyMsg';

procedure TPlayObject_SetHookSendUserStatusMsg; external LibName name 'TPlayObject_SetHookSendUserStatusMsg';

procedure TPlayObject_SetHookSendUserStruckMsg; external LibName name 'TPlayObject_SetHookSendUserStruckMsg';

procedure TPlayObject_SetHookSendUseMagicMsg; external LibName name 'TPlayObject_SetHookSendUseMagicMsg';

procedure TPlayObject_SetHookSendSocket; external LibName name 'TPlayObject_SetHookSendSocket';

procedure TPlayObject_SetHookSendGoodsList; external LibName name 'TPlayObject_SetHookSendGoodsList';

procedure TPlayObject_SetCheckClientDropItem; external LibName name 'TPlayObject_SetCheckClientDropItem';

procedure TPlayObject_SetCheckClientDealItem; external LibName name 'TPlayObject_SetCheckClientDealItem';

procedure TPlayObject_SetCheckClientStorageItem; external LibName name 'TPlayObject_SetCheckClientStorageItem';

procedure TPlayObject_SetCheckClientRepairItem; external LibName name 'TPlayObject_SetCheckClientRepairItem';

procedure TPlayObject_SetHookCheckUserItems; external LibName name 'TPlayObject_SetHookCheckUserItems';

procedure TPlayObject_SetHookRun; external LibName name 'TPlayObject_SetHookRun';

procedure TPlayObject_SetHookFilterMsg; external LibName name 'TPlayObject_SetHookFilterMsg';

function TPlayObject_GetPlayObjectTick; external LibName name 'TPlayObject_GetPlayObjectTick';

procedure TPlayObject_SetPlayObjectTick; external LibName name 'TPlayObject_SetPlayObjectTick';

function TPlayObject_CheckPlaySideNpc; external LibName name 'TPlayObject_CheckPlaySideNpc';

function TNormNpc_sFilePath; external LibName name 'TNormNpc_sFilePath';

function TNormNpc_sPath; external LibName name 'TNormNpc_sPath';

procedure TNormNpc_GetLineVariableText; external LibName name 'TNormNpc_GetLineVariableText';

procedure TNormNpc_SetScriptActionCmd; external LibName name 'TNormNpc_SetScriptActionCmd';

function TNormNpc_GetScriptActionCmd; external LibName name 'TNormNpc_GetScriptActionCmd';

procedure TNormNpc_SetScriptConditionCmd; external LibName name 'TNormNpc_SetScriptConditionCmd';

function TNormNpc_GetScriptConditionCmd; external LibName name 'TNormNpc_GetScriptConditionCmd';

procedure TNormNpc_SetScriptAction; external LibName name 'TNormNpc_SetScriptAction';

function TNormNpc_GetScriptAction; external LibName name 'TNormNpc_GetScriptAction';

procedure TNormNpc_SetScriptCondition; external LibName name 'TNormNpc_SetScriptCondition';

function TNormNpc_GetScriptCondition; external LibName name 'TNormNpc_GetScriptCondition';

function TNormNpc_GetManageNpc; external LibName name 'TNormNpc_GetManageNpc';

function TNormNpc_GetFunctionNpc; external LibName name 'TNormNpc_GetFunctionNpc';

procedure TNormNpc_GotoLable; external LibName name 'TNormNpc_GotoLable';

function TMerchant_GoodsList; external LibName name 'TMerchant_GoodsList';

function TMerchant_GetItemPrice; external LibName name 'TMerchant_GetItemPrice';

function TMerchant_GetUserPrice; external LibName name 'TMerchant_GetUserPrice';

function TMerchant_GetUserItemPrice; external LibName name 'TMerchant_GetUserPrice';

procedure TMerchant_SetHookClientGetDetailGoodsList; external LibName name 'TMerchant_SetHookClientGetDetailGoodsList';

function TMerchant_boSellOff; external LibName name 'TMerchant_boSellOff';

function TMapManager_FindMap; external LibName name 'TMapManager_FindMap';

function TEnvirnoment_GetRangeBaseObject; external LibName name 'TEnvirnoment_GetRangeBaseObject';

function TEnvirnoment_boCANRIDE; external LibName name 'TEnvirnoment_boCANRIDE';

function TEnvirnoment_boCANBAT; external LibName name 'TEnvirnoment_boCANBAT';

function TUserEngine_Create; external LibName name 'TUserEngine_Create';

procedure TUserEngine_Free; external LibName name 'TUserEngine_Free';

function TUserEngine_GetUserEngine; external LibName name 'TUserEngine_GetUserEngine';

function TUserEngine_GetPlayObject; external LibName name 'TUserEngine_GetPlayObject';

function TUserEngine_GetLoadPlayList; external LibName name 'TUserEngine_GetLoadPlayList';

function TUserEngine_GetPlayObjectList; external LibName name 'TUserEngine_GetPlayObjectList';

function TUserEngine_GetLoadPlayCount; external LibName name 'TUserEngine_GetLoadPlayCount';

function TUserEngine_GetPlayObjectCount; external LibName name 'TUserEngine_GetPlayObjectCount';

function TUserEngine_GetStdItemByName; external LibName name 'TUserEngine_GetStdItemByName';

function TUserEngine_GetStdItemByIndex; external LibName name 'TUserEngine_GetStdItemByIndex';

function TUserEngine_CopyToUserItemFromName; external LibName name 'TUserEngine_CopyToUserItemFromName';

function TUserEngine_GetStdItemList; external LibName name 'TUserEngine_GetStdItemList';

function TUserEngine_GetClientItem; external LibName name 'TUserEngine_GetClientItem';

procedure TUserEngine_SetHookRun; external LibName name 'TUserEngine_SetHookRun';

function TUserEngine_GetHookRun; external LibName name 'TUserEngine_GetHookRun';

procedure TUserEngine_SetHookClientUserMessage; external LibName name 'TUserEngine_SetHookClientUserMessage';

function TUserEngine_GetMapMonster; external LibName name 'TUserEngine_GetMapMonster';

function M2Share_GetNoClearMonList; external LibName name 'M2Share_GetNoClearMonList';

function TGuild_RankList; external LibName name 'TGuild_RankList';

function TItemUnit_GetItem; external LibName name 'TItemUnit_GetItem';

function TItemUnit_sItemName; external LibName name 'TItemUnit_sItemName';

function TItemUnit_nStdMode; external LibName name 'TItemUnit_nStdMode';

function TItemUnit_nSellOff; external LibName name 'TItemUnit_nSellOff';

function TItemUnit_nLevelItem; external LibName name 'TItemUnit_nLevelItem';

function TItemUnit_nItemType; external LibName name 'TItemUnit_nItemType';

function TItemUnit_nNeedIdentify; external LibName name 'TItemUnit_nNeedIdentify';

procedure TItemUnit_GetStandardItem; external LibName name 'TItemUnit_GetStandardItem';

procedure TItemUnit_GetItemAddValue; external LibName name 'TItemUnit_GetItemAddValue';

function TMagicManager_GetMagicManager; external LibName name 'TMagicManager_GetMagicManager';

function TMagicManager_DoSpell; external LibName name 'TMagicManager_DoSpell';

function TMagicManager_MPow; external LibName name 'TMagicManager_MPow';

function TMagicManager_GetPower; external LibName name 'TMagicManager_GetPower';

function TMagicManager_GetPower13; external LibName name 'TMagicManager_GetPower13';

function TMagicManager_GetRPow; external LibName name 'TMagicManager_GetRPow';

function TMagicManager_IsWarrSkill; external LibName name 'TMagicManager_IsWarrSkill';

function TMagicManager_MagBigHealing; external LibName name 'TMagicManager_MagBigHealing';

function TMagicManager_MagPushArround; external LibName name 'TMagicManager_MagPushArround';

function TMagicManager_MagPushArroundTaos; external LibName name 'TMagicManager_MagPushArroundTaos';

function TMagicManager_MagTurnUndead; external LibName name 'TMagicManager_MagTurnUndead';

function TMagicManager_MagMakeHolyCurtain; external LibName name 'TMagicManager_MagMakeHolyCurtain';

function TMagicManager_MagMakeGroupTransparent; external LibName name 'TMagicManager_MagMakeGroupTransparent';

function TMagicManager_MagTamming; external LibName name 'TMagicManager_MagTamming';

function TMagicManager_MagSaceMove; external LibName name 'TMagicManager_MagSaceMove';

function TMagicManager_MagMakeFireCross; external LibName name 'TMagicManager_MagMakeFireCross';

function TMagicManager_MagBigExplosion; external LibName name 'TMagicManager_MagBigExplosion';

function TMagicManager_MagElecBlizzard; external LibName name 'TMagicManager_MagElecBlizzard';

function TMagicManager_MabMabe; external LibName name 'TMagicManager_MabMabe';

function TMagicManager_MagMakeSlave; external LibName name 'TMagicManager_MagMakeSlave';

function TMagicManager_MagMakeSinSuSlave; external LibName name 'TMagicManager_MagMakeSinSuSlave';

function TMagicManager_MagWindTebo; external LibName name 'TMagicManager_MagWindTebo';

function TMagicManager_MagGroupLightening; external LibName name 'TMagicManager_MagGroupLightening';

function TMagicManager_MagGroupAmyounsul; external LibName name 'TMagicManager_MagGroupAmyounsul';

function TMagicManager_MagGroupDeDing; external LibName name 'TMagicManager_MagGroupDeDing';

function TMagicManager_MagGroupMb; external LibName name 'TMagicManager_MagGroupMb';

function TMagicManager_MagHbFireBall; external LibName name 'TMagicManager_MagHbFireBall';

function TMagicManager_MagLightening; external LibName name 'TMagicManager_MagLightening';

procedure TMagicManager_SetHookDoSpell; external LibName name 'TMagicManager_SetHookDoSpell';

procedure TRunSocket_CloseUser; external LibName name 'TRunSocket_CloseUser';

procedure TRunSocket_SetHookExecGateMsgOpen; external LibName name 'TRunSocket_SetHookExecGateMsgOpen';

procedure TRunSocket_SetHookExecGateMsgClose; external LibName name 'TRunSocket_SetHookExecGateMsgClose';

procedure TRunSocket_SetHookExecGateMsgEeceiveOK; external LibName name 'TRunSocket_SetHookExecGateMsgEeceiveOK';

procedure TRunSocket_SetHookExecGateMsgData; external LibName name 'TRunSocket_SetHookExecGateMsgData';


//将短字符类型的数据转换成PChar
//pszDest指向的字符大小在256个字符
procedure ShortStringToPChar(S: _LPTSHORTSTRING; pszDest: PAnsiChar);
begin
  Move(S.Strings, pszDest^, S.btLen);
  pszDest[S.btLen] := #0;
end;

function MakeDefaultMsg(wIdent: Word; nRecog: Integer; wParam, wTag, wSeries: Word): _TDEFAULTMESSAGE;
begin
  Result.nRecog := nRecog;
  Result.wIdent := wIdent;
  Result.wParam := wParam;
  Result.wTag := wTag;
  Result.wSeries := wSeries;
end;

//解密
function DecodeMessage(str: string): _TDEFAULTMESSAGE;
var
  EncBuf: array[0..BUFFERSIZE - 1] of AnsiChar;
  Msg: _TDEFAULTMESSAGE;
begin
  EDcode_Decode6BitBuf(PAnsiChar(AnsiString(str)), @EncBuf, Length(AnsiString(str)), SizeOf(EncBuf));
  Move(EncBuf, Msg, sizeof(_TDEFAULTMESSAGE));
  Result := Msg;
end;

function DecodeString(str: string): string;
var
  EncBuf: array[0..BUFFERSIZE - 1] of AnsiChar;
begin
  EDcode_Decode6BitBuf(PAnsiChar(AnsiString(str)), @EncBuf, Length(AnsiString(str)), SizeOf(EncBuf));
  Result := string(EncBuf);
end;

procedure DecodeBuffer(src: string; buf: PAnsiChar; bufsize: integer);
var
  EncBuf: array[0..BUFFERSIZE - 1] of AnsiChar;
begin
  EDcode_Decode6BitBuf(PAnsiChar(AnsiString(src)), @EncBuf, Length(AnsiString(src)), SizeOf(EncBuf));
  Move(EncBuf, buf^, bufsize);
end;

//加密
function EncodeString(str: string): string;
var
  EncBuf: array[0..BUFFERSIZE - 1] of AnsiChar;
begin
  EDcode_Encode6BitBuf(PAnsiChar(AnsiString(str)), @EncBuf, Length(AnsiString(str)), SizeOf(EncBuf));
  Result := string(EncBuf);
end;

function EncodeBuffer(buf: PAnsiChar; bufsize: integer): string;
var
  EncBuf, TempBuf: array[0..BUFFERSIZE - 1] of AnsiChar;
begin
  if bufsize < BUFFERSIZE then
  begin
    Move(buf^, TempBuf, bufsize);
    EDcode_Encode6BitBuf(@TempBuf, @EncBuf, bufsize, SizeOf(EncBuf));
    Result := string(EncBuf);
  end
  else
    Result := '';
end;

function EncodeMessage(smsg: _TDEFAULTMESSAGE): string;
var
  EncBuf, TempBuf: array[0..BUFFERSIZE - 1] of AnsiChar;
begin
  Move(smsg, TempBuf, sizeof(_TDEFAULTMESSAGE));
  EDcode_Encode6BitBuf(@TempBuf, @EncBuf, sizeof(_TDEFAULTMESSAGE), SizeOf(EncBuf));
  Result := string(EncBuf);
end;

end.

