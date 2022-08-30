// CodeGear C++Builder
// Copyright (c) 1995, 2013 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'CnHashTable.pas' rev: 26.00 (Windows)

#ifndef CnhashtableHPP
#define CnhashtableHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Cnhashtable
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCnBucket;
class PASCALIMPLEMENTATION TCnBucket : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
protected:
	virtual int __fastcall CompareStrings(const System::UnicodeString S1, const System::UnicodeString S2);
	
public:
	__fastcall TCnBucket(const int InitCapacity);
	virtual int __fastcall AddObject(const System::UnicodeString S, System::TObject* AObject);
	int __fastcall EnsureAddObject(const System::UnicodeString S, System::TObject* AObject);
public:
	/* TStringList.Destroy */ inline __fastcall virtual ~TCnBucket(void) { }
	
};


typedef System::DynamicArray<TCnBucket*> TCnBucketDynArray;

class DELPHICLASS TCnHashTable;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCnHashTable : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	typedef System::DynamicArray<int> _TCnHashTable__1;
	
	
private:
	int FUpdateCount;
	int FCount;
	_TCnHashTable__1 FBucketCounts;
	int FAutoRehashPoint;
	TCnBucket* FSortedList;
	void __fastcall DoRehash(const int iCount);
	void __fastcall Rehash(void);
	void __fastcall NeedRebuildBucketCounts(void);
	
protected:
	int FRehashCount;
	virtual int __fastcall GetCount(void);
	virtual System::UnicodeString __fastcall GetKeys(const int Index);
	virtual int __fastcall GetNewBucketCount(int OldSize);
	TCnBucket* __fastcall Find(const System::UnicodeString s);
	virtual unsigned __fastcall HashOf(const System::UnicodeString s);
	virtual int __fastcall LimitBucketCount(int i);
	void __fastcall BuildBucketCounts(void);
	virtual void __fastcall RehashTo(int NewSize, const int InitCapacity = 0x0);
	virtual void __fastcall SetUpdateState(bool Updating);
	__property int UpdateCount = {read=FUpdateCount, nodefault};
	
public:
	TCnBucketDynArray Buckets;
	int FBucketCount;
	__fastcall TCnHashTable(const int BucketSize, const int InitCapacity);
	__fastcall virtual ~TCnHashTable(void);
	virtual bool __fastcall Exists(const System::UnicodeString s);
	virtual int __fastcall ExistsPos(const System::UnicodeString s);
	virtual System::TObject* __fastcall GetValues(const System::UnicodeString s);
	virtual void __fastcall SetValues(const System::UnicodeString s, System::TObject* Value);
	virtual System::UnicodeString __fastcall Info(void);
	void __fastcall BeginUpdate(void);
	void __fastcall EndUpdate(void);
	virtual void __fastcall Add(const System::UnicodeString s, System::TObject* obj);
	virtual void __fastcall Clear(void);
	virtual void __fastcall Delete(const System::UnicodeString s);
	virtual void __fastcall Put(const System::UnicodeString s, System::TObject* obj);
	void __fastcall BuildSortedList(void);
	__property int AutoRehashPoint = {read=FAutoRehashPoint, write=FAutoRehashPoint, default=128};
	__property int Count = {read=GetCount, nodefault};
	__property System::UnicodeString Keys[const int Index] = {read=GetKeys};
	__property System::TObject* Values[const System::UnicodeString Index] = {read=GetValues, write=SetValues};
	__property TCnBucket* SortedList = {read=FSortedList};
};

#pragma pack(pop)

class DELPHICLASS TCnHashTableSmall;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCnHashTableSmall : public TCnHashTable
{
	typedef TCnHashTable inherited;
	
protected:
	virtual unsigned __fastcall HashOf(const System::UnicodeString s);
	virtual void __fastcall RehashTo(int NewSize, const int InitCapacity = 0x0);
	
public:
	__fastcall TCnHashTableSmall(const int InitCapacity);
public:
	/* TCnHashTable.Destroy */ inline __fastcall virtual ~TCnHashTableSmall(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TCnHashTableMedium;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCnHashTableMedium : public TCnHashTable
{
	typedef TCnHashTable inherited;
	
protected:
	virtual unsigned __fastcall HashOf(const System::UnicodeString s);
	virtual void __fastcall RehashTo(int NewSize, const int InitCapacity = 0x0);
	
public:
	__fastcall TCnHashTableMedium(const int InitCapacity);
public:
	/* TCnHashTable.Destroy */ inline __fastcall virtual ~TCnHashTableMedium(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TCnHashTableBig;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TCnHashTableBig : public TCnHashTable
{
	typedef TCnHashTable inherited;
	
protected:
	virtual unsigned __fastcall HashOf(const System::UnicodeString s);
	virtual void __fastcall RehashTo(int NewSize, const int InitCapacity = 0x0);
	
public:
	__fastcall TCnHashTableBig(const int InitCapacity);
public:
	/* TCnHashTable.Destroy */ inline __fastcall virtual ~TCnHashTableBig(void) { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Byte DefaultAutoRehashPoint = System::Byte(0x80);
extern DELPHI_PACKAGE int MaxBucketsCount;
extern DELPHI_PACKAGE int MinBucketsCount;
}	/* namespace Cnhashtable */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_CNHASHTABLE)
using namespace Cnhashtable;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// CnhashtableHPP
