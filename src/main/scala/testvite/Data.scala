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
      "subject": "v-10",
      "object": "v-9",
      "predicate": "has a"
    },
    {
      "id": "r-7",
      "subject": "v-1",
      "object": "r-8",
      "predicate": "actor role"
    },
    {
      "subject": "v-7",
      "object": "v-8",
      "predicate": "has a"
    },
    {
      "subject": "v-1",
      "object": "r-10",
      "predicate": "actor role"
    },
    {
      "id": "r-5",
      "subject": "v-1",
      "object": "r-6",
      "predicate": "actor role"
    },
    {
      "subject": "v-2",
      "object": "r-2",
      "predicate": "has a"
    },
    {
      "subject": "v-13",
      "object": "v-6",
      "predicate": "character"
    },
    {
      "subject": "v-1",
      "object": "r-12",
      "predicate": "actor role"
    },
    {
      "subject": "v-2",
      "object": "r-4",
      "predicate": "has a"
    },
    {
      "subject": "v-12",
      "object": "v-4",
      "predicate": "character"
    },
    {
      "subject": "v-12",
      "object": "v-3",
      "predicate": "actor"
    },
    {
      "subject": "v-13",
      "object": "v-5",
      "predicate": "actor"
    }
  ]
}
  """