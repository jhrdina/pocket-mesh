open BlackTea;
open Json.Infix;

/* TYPES */

type timestamp;
type opSet;

type groupStatus = {
  id: PeerGroup.Id.t,
  clock: PeerGroup.AM.Clock.t,
  permissions: PeerGroup.groupPermissions,
  permissionsClock: PeerGroup.AM.Clock.t,
};

type groupChangesRequest =
  | Content(PeerGroup.AM.Clock.t)
  | Members(PeerGroup.AM.Clock.t)
  | ContentAndMembers(PeerGroup.AM.Clock.t, PeerGroup.AM.Clock.t);

/*

 Content
 |   Permissions
 |   |
 ------
 x   x     Equivalent of not being in the list at all
 x   r     [weird]
 x   rw    [weird]
 r   x     [weird]
 r   r
 r   rw    [weird, he can grant himself permissions]
 rw  x     [weird, he can still get the list of all peers that participated.
            He just doesn't know which of them currently have read or write.]
 rw  r
 rw  rw    Peer is an authoritative server

 ------
 r   r
 rw  r
 rw  rw

 */

type blobInfo = {
  /* Really timestamp? How about moving blob between different parts of the
     JSON tree? I don't want it to be copied everytime...
     Alternatives: GUID, hash */
  id: timestamp,
  length: int,
  blockSize: int,
};

type groupChanges =
  | Content(PeerGroup.AM.ChangeSet.t)
  | Members(PeerGroup.AM.ChangeSet.t)
  | ContentAndMembers(PeerGroup.AM.ChangeSet.t, PeerGroup.AM.ChangeSet.t);

type t =
  | ChangesOffer(PeerGroup.Id.Map.t(groupStatus))
  | ChangesRequest(PeerGroup.Id.Map.t(groupChangesRequest))
  | Changes(PeerGroup.Id.Map.t(groupChanges));
/*| Blob(blobInfo)*/

/* SERIALIZATION */

let encodeGroupStatus = (t: groupStatus) =>
  Json.(
    Object([
      ("id", String(t.id |> PeerGroup.Id.toString)),
      ("clock", String(t.clock |> PeerGroup.AM.Clock.toString)),
      ("permissions", String(t.permissions |> PeerGroup.encodePermissions)),
      (
        "permissionsClock",
        String(t.permissionsClock |> PeerGroup.AM.Clock.toString),
      ),
    ])
  );
let decodeGroupStatus = json =>
  Json.(
    switch (
      json |> get("id") |?> string |?> PeerGroup.Id.ofString,
      json |> get("clock") |?> string |?> PeerGroup.AM.Clock.fromString,
      json |> get("permissions") |?> string |?> PeerGroup.decodePermissions,
      json
      |> get("permissionsClock")
      |?> string
      |?> PeerGroup.AM.Clock.fromString,
    ) {
    | (Some(id), Some(clock), Some(permissions), Some(permissionsClock)) =>
      Some({id, clock, permissions, permissionsClock})
    | _ => None
    }
  );

let encodeGroupChangesRequest = (t: groupChangesRequest) =>
  Json.(
    switch (t) {
    | Content(contentClock) =>
      Array([
        String("c"),
        String(PeerGroup.AM.Clock.toString(contentClock)),
      ])
    | Members(membersClock) =>
      Array([
        String("m"),
        String(PeerGroup.AM.Clock.toString(membersClock)),
      ])
    | ContentAndMembers(contentClock, membersClock) =>
      Array([
        String("cm"),
        String(PeerGroup.AM.Clock.toString(contentClock)),
        String(PeerGroup.AM.Clock.toString(membersClock)),
      ])
    }
  );
let decodeGroupChangesRequest: Json.t => option(groupChangesRequest) =
  json =>
    Json.(
      switch (
        json |> nth(0) |?> string,
        json |> nth(1) |?> string |?> PeerGroup.AM.Clock.fromString,
        json |> nth(2) |?> string |?> PeerGroup.AM.Clock.fromString,
      ) {
      | (Some("c"), Some(contentClock), None) =>
        Some(Content(contentClock))
      | (Some("m"), Some(membersClock), None) =>
        Some(Members(membersClock))
      | (Some("cm"), Some(contentClock), Some(membersClock)) =>
        Some(ContentAndMembers(contentClock, membersClock))
      | _ => None
      }
    );

let encodeGroupChanges = t =>
  Json.(
    switch (t) {
    | Content(contentChanges) =>
      Array([
        String("c"),
        String(PeerGroup.AM.ChangeSet.toString(contentChanges)),
      ])
    | Members(membersChanges) =>
      Array([
        String("m"),
        String(PeerGroup.AM.ChangeSet.toString(membersChanges)),
      ])
    | ContentAndMembers(contentChanges, membersChanges) =>
      Array([
        String("cm"),
        String(PeerGroup.AM.ChangeSet.toString(contentChanges)),
        String(PeerGroup.AM.ChangeSet.toString(membersChanges)),
      ])
    }
  );
let decodeGroupChanges = json =>
  Json.(
    switch (
      json |> nth(0) |?> string,
      json |> nth(1) |?> string |?> PeerGroup.AM.ChangeSet.fromString,
      json |> nth(2) |?> string |?> PeerGroup.AM.ChangeSet.fromString,
    ) {
    | (Some("c"), Some(contentChanges), None) =>
      Some(Content(contentChanges))
    | (Some("m"), Some(membersChanges), None) =>
      Some(Members(membersChanges))
    | (Some("cm"), Some(contentChanges), Some(membersChanges)) =>
      Some(ContentAndMembers(contentChanges, membersChanges))
    | _ => None
    }
  );

let encodePeerGroupIdMap = (itemEncoder, map) =>
  Rex_json.Json.Object(
    PeerGroup.Id.Map.fold(
      (groupId, value, acc) =>
        [(groupId |> PeerGroup.Id.toString, itemEncoder(value)), ...acc],
      map,
      [],
    ),
  );
let decodePeerGroupIdMap = (itemDecoder, json) =>
  json
  |> Json.obj
  |?> List.fold_left(
        (maybeNewMap, (groupIdStr, value)) =>
          switch (maybeNewMap) {
          | Some(newMap) =>
            switch (PeerGroup.Id.ofString(groupIdStr), itemDecoder(value)) {
            | (Some(groupId), Some(decodedValue)) =>
              Some(newMap |> PeerGroup.Id.Map.add(groupId, decodedValue))
            | _ => None
            }
          | None => None
          },
        Some(PeerGroup.Id.Map.empty),
      );

let encode = t => {
  let (typeStr, payload) =
    switch (t) {
    | ChangesOffer(groupsChangesOffer) => (
        "changesOffer",
        encodePeerGroupIdMap(encodeGroupStatus, groupsChangesOffer),
      )
    | ChangesRequest(groupsChangesRequest) => (
        "changesRequest",
        encodePeerGroupIdMap(encodeGroupChangesRequest, groupsChangesRequest),
      )
    | Changes(groupsChanges) => (
        "changes",
        encodePeerGroupIdMap(encodeGroupChanges, groupsChanges),
      )
    };

  Rex_json.Json.(
    Object([("type", String(typeStr)), ("payload", payload)])
  );
};
let decode = json =>
  Json.(
    switch (json |> get("type") |?> string, json |> get("payload")) {
    | (Some("changesOffer"), Some(payload)) =>
      switch (decodePeerGroupIdMap(decodeGroupStatus, payload)) {
      | Some(groupsStatuses) => Some(ChangesOffer(groupsStatuses))
      | None => None
      }

    | (Some("changesRequest"), Some(payload)) =>
      switch (decodePeerGroupIdMap(decodeGroupChangesRequest, payload)) {
      | Some(groupsChangesRequest) =>
        Some(ChangesRequest(groupsChangesRequest))
      | None => None
      }

    | (Some("changes"), Some(payload)) =>
      switch (decodePeerGroupIdMap(decodeGroupChanges, payload)) {
      | Some(groupsChanges) => Some(Changes(groupsChanges))
      | None => None
      }

    | _ => None
    }
  );

/*
  How do I (A) send changes to the other peer (B)?
  - I did the change
    - I send it immediately to everybody who is interested & online
  - Someone else (C) did the change, I've only received it
    - Check if their permissions match (B allows C to write and C allows B to read)
    - if match, check if C is online so that he can propagate the changes himself
    - if he is not online, send changes of C to B.
    - in any other case don't propagate the changes (B doesn't care about C's changes, C doesn't want B to read his changes (you would be the bad informer) )
 */