create extension if not exists citext;
create extension if not exists pgcrypto;

create table if not exists users (
  id bigserial primary key not null,
  name citext not null unique,
  pass text not null,
  email citext not null unique,
  image text not null,
  bio text not null
);

create table if not exists articles (
  id bigserial primary key not null,
  slug citext not null unique,
  title text not null,
  description text not null,
  body text not null,
  created_at timestamptz not null,
  updated_at timestamptz not null,
  author_id bigserial references users(id),
  tags citext[] not null
);

create table if not exists followings (
  user_id bigserial references users(id),
  followed_by bigserial references users(id),
  primary key (user_id, followed_by)
);

create table if not exists favorites (
  article_id bigserial references articles(id),
  favorited_by bigserial references users(id),
  primary key (article_id, favorited_by)
);

create table if not exists comments (
  article_id bigserial references articles(id),
  id bigserial primary key,
  created_at timestamptz not null,
  updated_at timestamptz not null,
  body text not null,
  author_id bigserial references users(id)
);