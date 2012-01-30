module Data.ContentMathML3.XNodeOrd
where
  
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.DOM.QualifiedName
import Data.Data
import Data.Tree.NTree.TypeDefs

instance Ord XNode where
  (XText v1) `compare` (XText v2) = v1 `compare` v2
  (XText _) `compare` _ = LT
  _ `compare` (XText _) = GT
  
  (XBlob v1) `compare` (XBlob v2) = v1 `compare` v2
  (XBlob _) `compare` _ = LT
  _ `compare` (XBlob _) = GT
  
  (XCharRef v1) `compare` (XCharRef v2) = v1 `compare` v2
  (XCharRef _) `compare` _ = LT
  _ `compare` (XCharRef _) = GT

  (XEntityRef v1) `compare` (XEntityRef v2) = v1 `compare` v2
  (XEntityRef _) `compare` _ = LT
  _ `compare` (XEntityRef _) = GT

  (XCmt v1) `compare` (XCmt v2) = v1 `compare` v2
  (XCmt _) `compare` _ = LT
  _ `compare` (XCmt _) = GT

  (XCdata v1) `compare` (XCdata v2) = v1 `compare` v2
  (XCdata _) `compare` _ = LT
  _ `compare` (XCdata _) = GT

  (XPi qn1 t1) `compare` (XPi qn2 t2) =
    let c1 = qn1 `compare` qn2
    in
     case c1
       of
        EQ -> t1 `compare` t2
        _ -> c1
  (XPi _ _) `compare` _ = LT
  _ `compare` (XPi _ _) = GT

  (XTag qn1 t1) `compare` (XTag qn2 t2) =
    let c1 = qn1 `compare` qn2
    in
     case c1
       of
        EQ -> t1 `compare` t2
        _ -> c1
  (XTag _ _) `compare` _ = LT
  _ `compare` (XTag _ _) = GT

  (XDTD qn1 t1) `compare` (XDTD qn2 t2) =
    let c1 = qn1 `compare` qn2
    in
     case c1
       of
        EQ -> t1 `compare` t2
        _ -> c1
  (XDTD _ _) `compare` _ = LT
  _ `compare` (XDTD _ _) = GT

  (XAttr v1) `compare` (XAttr v2) = v1 `compare` v2
  (XAttr _) `compare` _ = LT
  _ `compare` (XAttr _) = GT

  (XError qn1 t1) `compare` (XError qn2 t2) =
    let c1 = qn1 `compare` qn2
    in
     case c1
       of
        EQ -> t1 `compare` t2
        _ -> c1

instance Ord QName where
  q1 `compare` q2 =
    let
      c1 = (localPart q1) `compare` (localPart q2)
      c2 = (namePrefix q1) `compare` (namePrefix q2)
      c3 = (namespaceUri q1) `compare` (namespaceUri q2)
    in
     case c1 of
       EQ ->
         case c2 of
           EQ -> c3
           _ -> c2
       _ -> c1

instance Data XNode where
  gfoldl k z (XText a) = z XText `k` a
  gfoldl k z (XBlob a) = z XBlob `k` a
  gfoldl k z (XCharRef a) = z XCharRef `k` a
  gfoldl k z (XEntityRef a) = z XEntityRef `k` a
  gfoldl k z (XCmt a) = z XCmt `k` a
  gfoldl k z (XCdata a) = z XCdata `k` a
  gfoldl k z (XPi a b) = z XPi `k` a `k` b
  gfoldl k z (XTag a b) = z XTag `k` a `k` b
  gfoldl k z (XDTD a b) = z XDTD `k` a `k` b
  gfoldl k z (XAttr a) = z XAttr `k` a
  gfoldl k z (XError a b) = z XError `k` a `k` b
  
  gunfold k z c = case constrIndex c of
       1 -> k (z XText)
       2 -> k (z XBlob)
       3 -> k (z XCharRef)
       4 -> k (z XEntityRef)
       5 -> k (z XCmt)
       6 -> k (z XCdata)
       7 -> k (k (z XPi))
       8 -> k (k (z XTag))
       9 -> k (k (z XDTD))
       10 -> k (z XAttr)
       11 -> k (k (z XError))
  toConstr (XText _) = con_XText
  toConstr (XBlob _) = con_XBlob
  toConstr (XCharRef _) = con_XCharRef
  toConstr (XEntityRef _) = con_XEntityRef
  toConstr (XCmt _) = con_XCmt
  toConstr (XCdata _) = con_XCdata
  toConstr (XPi _ _) = con_XPi
  toConstr (XTag _ _) = con_XTag
  toConstr (XDTD _ _) = con_XDTD
  toConstr (XAttr _) = con_XAttr
  toConstr (XError _ _) = con_XError

  dataTypeOf _ = ty_XNode

con_XText = mkConstr ty_XNode "XText" [] Prefix
con_XBlob = mkConstr ty_XNode "XBlob" [] Prefix
con_XCharRef = mkConstr ty_XNode "XCharRef" [] Prefix
con_XEntityRef = mkConstr ty_XNode "XEntityRef" [] Prefix
con_XCmt = mkConstr ty_XNode "XCmt" [] Prefix
con_XCdata = mkConstr ty_XNode "XCdata" [] Prefix
con_XPi = mkConstr ty_XNode "XPi" [] Prefix
con_XTag = mkConstr ty_XNode "XTag" [] Prefix
con_XDTD = mkConstr ty_XNode "XDTD" [] Prefix
con_XAttr = mkConstr ty_XNode "XAttr" [] Prefix
con_XError = mkConstr ty_XNode "XError" [] Prefix
ty_XNode   = mkDataType "Text.XML.HXT.DOM.TypeDefs.XNode" [con_XText, con_XBlob, con_XCharRef, con_XEntityRef, con_XCmt, con_XCdata, con_XPi,
                                                           con_XTag, con_XDTD, con_XAttr, con_XError]
instance Data n => Data (NTree n) where
  gfoldl k z (NTree a b) = z NTree `k` a `k` b
  gunfold k z c = case constrIndex c of
    1 -> k (k (z NTree))
  toConstr (NTree _ _) = con_NTree
  dataTypeOf _ = ty_NTree
con_NTree = mkConstr ty_NTree "NTree" [] Prefix
ty_NTree = mkDataType "Data.Tree.NTree.TypeDefs.NTree" [con_NTree]

instance Data DTDElem where
  gfoldl k z v = z v
  gunfold k z c = case constrIndex c of
    1 -> z DOCTYPE
    2 -> z ELEMENT
    3 -> z CONTENT
    4 -> z ATTLIST
    5 -> z ENTITY
    6 -> z PENTITY
    7 -> z NOTATION
    8 -> z CONDSECT
    9 -> z NAME
    10 -> z PEREF
  toConstr DOCTYPE = con_DOCTYPE
  toConstr ELEMENT = con_ELEMENT
  toConstr CONTENT = con_CONTENT
  toConstr ATTLIST = con_ATTLIST
  toConstr ENTITY = con_ENTITY
  toConstr PENTITY = con_PENTITY
  toConstr NOTATION = con_NOTATION
  toConstr CONDSECT = con_CONDSECT
  toConstr NAME = con_NAME
  toConstr PEREF = con_PEREF
  
  dataTypeOf _ = ty_DTDElem

con_DOCTYPE = mkConstr ty_DTDElem "DOCTYPE" [] Prefix
con_ELEMENT = mkConstr ty_DTDElem "ELEMENT" [] Prefix
con_CONTENT = mkConstr ty_DTDElem "CONTENT" [] Prefix
con_ATTLIST = mkConstr ty_DTDElem "ATTLIST" [] Prefix
con_ENTITY = mkConstr ty_DTDElem "ENTITY" [] Prefix
con_PENTITY = mkConstr ty_DTDElem "PENTITY" [] Prefix
con_NOTATION = mkConstr ty_DTDElem "NOTATION" [] Prefix
con_CONDSECT = mkConstr ty_DTDElem "CONDSECT" [] Prefix
con_NAME = mkConstr ty_DTDElem "NAME" [] Prefix
con_PEREF = mkConstr ty_DTDElem "PEREF" [] Prefix

ty_DTDElem = mkDataType "Text.XML.HXT.DOM.TypeDefs.DTDElem" [con_DOCTYPE, con_ELEMENT, con_CONTENT, con_ATTLIST, con_ENTITY, con_PENTITY, con_NOTATION, con_CONDSECT, con_NAME, con_PEREF]

instance Data QName where
  gfoldl k z v = z mkQName `k` (localPart v) `k` (namePrefix v) `k` (namespaceUri v)
  gunfold k z c = case constrIndex c of
    1 -> k (k (k (z mkQName)))
  toConstr v = con_QName
  dataTypeOf _ = ty_QName

con_QName = mkConstr ty_QName "QName" [] Prefix
ty_QName = mkDataType "Text.XML.HXT.DOM.TypeDefs.QName" [con_QName]
