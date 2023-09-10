package testvite

val data = 
  """
  {
  "values": {
    "v-1": "Arrival",
    "v-4": "Louise Banks",
    "v-10": "gr ac role character",
    "v-5": "Jeremy Renner",
    "v-6": "Ian Donnelly",
    "v-12": "gr ac role role 1",
    "v-3": "Amy Adams",
    "v-2": "Gravity",
    "v-7": "gr ac role 1",
    "v-13": "gr ac role ac 2",
    "v-9": "Ryan Stone",
    "v-8": "Sandra Bullock"
  },
  "relations": [
    {
      "id": "r-4",
      "from": "v-10",
      "to": "v-9",
      "kind": "has a"
    },
    {
      "id": "r-7",
      "from": "v-1",
      "to": "r-8",
      "kind": "actor role"
    },
    {
      "id": "r-2",
      "from": "v-7",
      "to": "v-8",
      "kind": "has a"
    },
    {
      "id": "r-9",
      "from": "v-1",
      "to": "r-10",
      "kind": "actor role"
    },
    {
      "id": "r-5",
      "from": "v-1",
      "to": "r-6",
      "kind": "actor role"
    },
    {
      "id": "r-1",
      "from": "v-2",
      "to": "r-2",
      "kind": "has a"
    },
    {
      "id": "r-12",
      "from": "v-13",
      "to": "v-6",
      "kind": "character"
    },
    {
      "id": "r-11",
      "from": "v-1",
      "to": "r-12",
      "kind": "actor role"
    },
    {
      "id": "r-3",
      "from": "v-2",
      "to": "r-4",
      "kind": "has a"
    },
    {
      "id": "r-8",
      "from": "v-12",
      "to": "v-4",
      "kind": "character"
    },
    {
      "id": "r-6",
      "from": "v-12",
      "to": "v-3",
      "kind": "actor"
    },
    {
      "id": "r-10",
      "from": "v-13",
      "to": "v-5",
      "kind": "actor"
    }
  ]
}
  """